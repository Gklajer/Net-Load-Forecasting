rm(list = objects()); graphics.off()

library(lubridate)
library(forecast)
library(magrittr)
library(mgcv)
library(quantreg)
library(ranger)
library(slider)
library(tidyverse)
library(yarrr)
source("Utils/score.R")
source("Utils/utils.R")

####

Data0 <- read_delim("Data/train.csv", delim = ",")
Data1 <- read_delim("Data/test.csv", delim = ",")

DataCOVID0 <- read_delim("Data/OxCGRT_simplified_fr/Data0-COVID_Indicators.csv", delim = ",")
DataCOVID1 <- read_delim("Data/OxCGRT_simplified_fr/Data1-COVID_Indicators.csv", delim = ",")

Data0 <- merge(Data0, DataCOVID0, by="Date")
Data1 <- merge(Data1, DataCOVID1, by="Date")

Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

Data0$WeekDays <- as.factor(Data0$WeekDays)
Data1$WeekDays <- as.factor(Data1$WeekDays)

sel_a <- which(Data0$Year <= 2021)
sel_b <- -sel_a

##### regroupement de modalités
Data0$WeekDaysGrouped <- forcats::fct_recode(weekdays(Data0$Date), "WorkDay" = "Thursday", "WorkDay" = "Tuesday", "WorkDay" = "Wednesday", "WorkDay" = "Friday")
Data1$WeekDaysGrouped <- forcats::fct_recode(weekdays(Data1$Date), "WorkDay" = "Thursday", "WorkDay" = "Tuesday", "WorkDay" = "Wednesday", "WorkDay" = "Friday")

#### Temperatures Spline Features
Data0$Temp_trunc1 <- pmax(Data0$Temp - 285, 0)
Data0$Temp_trunc2 <- pmax(Data0$Temp - 295, 0)

Data1$Temp_trunc1 <- pmax(Data1$Temp - 285, 0)
Data1$Temp_trunc2 <- pmax(Data1$Temp - 295, 0)


### Temperature ressentie

Data0$Temp_res_siple <- (33 + (Data0$Temp - 273.15 - 33)*(0.474 + 0.454*sqrt(Data0$Wind)) - 0.0454*Data0$Wind)+273.15 

Data0$Temp_res_windchill <- (13.12 + 0.6215*(Data0$Temp - 273.15) - 11.37*(Data0$Wind*3.6)**0.16 + 0.3965*(Data0$Temp - 273.15)*(Data0$Wind*3.6)**0.16) + 273.15

#### Fourier features
w <- 2 * pi / (365)
Nfourier <- 10

for (i in c(1:Nfourier))
{
  assign(paste("cos", i, sep = ""), cos(w * Data0$Time * i))
  assign(paste("sin", i, sep = ""), sin(w * Data0$Time * i))
}

cos <- paste(c("cos"), c(1:Nfourier), sep = "")
sin <- paste(c("sin"), c(1:Nfourier), sep = "")

Data0$Fourier10 <- eval(parse(text = paste(c(cos, sin), collapse = "+")))

for (i in c(1:Nfourier))
{
  assign(paste("cos", i, sep = ""), cos(w * Data1$Time * i))
  assign(paste("sin", i, sep = ""), sin(w * Data1$Time * i))
}

cos <- paste(c("cos"), c(1:Nfourier), sep = "")
sin <- paste(c("sin"), c(1:Nfourier), sep = "")

Data1$Fourier10 <- eval(parse(text = paste(c(cos, sin), collapse = "+")))

rm(list = c(cos, sin))

### Nébulosité
sel_c <- which(Data0$Year >= 2018)

Data0$Nebulosity_transformed = Data0$Nebulosity_weighted
Data0$Nebulosity_transformed[-sel_c] = Data0$Nebulosity_transformed[-sel_c] * sqrt(var(Data0$Nebulosity_transformed[sel_c]) / var(Data0$Nebulosity_transformed[-sel_c]))
Data0$Nebulosity_transformed[-sel_c] = Data0$Nebulosity_transformed[-sel_c] + mean(Data0$Nebulosity_transformed[sel_c]) - mean(Data0$Nebulosity_transformed[-sel_c])

Data1$Nebulosity_transformed = Data1$Nebulosity_weighted
###############################################################################################################################################################
########################################
##### RF
########################################

rf<- ranger::ranger(Net_demand~.-Load-Solar_power-Wind_power, data = Data0[sel_a,], importance= 'permutation')

imp <- sort(rf$variable.importance, decreasing = T)

par(mfrow=c(1, 1), mar=c(2, 2, 2, 0))
barplot(imp, axisnames = F, ylim = c(0, max(imp) + max(imp)/3), ylab='Importance (permutation)')
text(1:length(imp) * 1.2, imp+max(imp/5), labels = names(imp), srt=90, pos=3, adj=1)

########################################
##### Linear Models
########################################

eq <- Net_demand ~ WeekDays + Temp + Temp_trunc1 + Temp_trunc2 + Fourier10

######### Additional features

eq <- Net_demand ~ Fourier10 + 
  WeekDays + Holiday + BH + BH_before + 
  Net_demand.1 + Net_demand.7 + 
  Temp + Temp_trunc1 + Temp_trunc2 + 
  Nebulosity + Solar_power.1 +
  Wind + Wind_power.1 +
  StringencyIndex_Average

mod.lm <- lm(eq, data = Data0[sel_a,])
summary(mod.lm)

k = 7
testset_size = nrow(Data1)
trainset_size = nrow(Data0) - k * testset_size
cv1 = time_cv(lm, eq, Data0, trainset_size, testset_size, type = "window")

cv = cv1

par(mfrow=c(2, 2))
boxplot(cv$res$mean, ylab="in MW", main="mean"); boxplot(cv$res$quant, main="quant")
boxplot(cv$res$rmse, ylab="in MW", main="rmse"); boxplot(cv$res$pb, main="pb")

res = c(t(cv$res$val))

par(mfrow=c(1, 1))
qqnorm(res); qqline(res)
hist(res, breaks = 50)
plot(res, type="l")

########################################
##### Quantile regression
########################################

mod.rq <- rq(eq, data = Data0[sel_a,])
summary(mod.rq)

cv = time_cv(rq, eq, Data0, trainset_size, testset_size, type = "window", is.rq = T, tau = .95)

par(mfrow=c(1, 1))
boxplot(cv$res$pb, ylab="in MW", main="pb")

########################################
##### GAMs
########################################

eq <- Net_demand ~ s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') +
  WeekDays + BH +
  s(Net_demand.1, bs='cr') +  s(Net_demand.7, bs='cr') +
  s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp,k=10, bs='cr') + s(Temp_s99,k=10, bs='cr') + 
  s(Wind) + te(as.numeric(Date), Nebulosity, k=c(4,10))

######### Additional features

eq <- Net_demand ~ s(Time, k=3, bs='cr') + s(toy,k=30, bs='cc') +
  WeekDays * BH_before  + BH_Holiday + Holiday + 
  StringencyIndex_Average * EconomicSupportIndex +
  s(Load.1, by=WeekDays, bs='cr') + s(Load.7, bs='cr') +
  s(Wind_power.1, k=5, bs="cr") + s(Solar_power.1, k=5, bs='cr') +
  ti(Wind_weighted, k=10, bs='cr') + s(Nebulosity, by=Year, bs="cr") +
  ti(Temp, k=7, bs='cr') + ti(Temp, Temp_s99_max, bs='cr') + 
  ti(Wind_weighted, Temp, k=c(3,7), bs="cr")
  
mod.gam <- gam(eq,  data=Data0[sel_a,])
summary(mod.gam)
## gam.check(mod.gam)

k = 6
testset_size = nrow(Data1)
trainset_size = nrow(Data0) - k * testset_size
cv2 = time_cv(gam, eq, Data0, trainset_size, testset_size, type = "window")

cv = cv2

par(mfrow=c(2, 2), mar=c(1, 2, 1, 1))
boxplot(cv$res$mean, ylab="in MW", main="mean"); boxplot(cv$res$quant, main="quant")
boxplot(cv$res$rmse, ylab="in MW", main="rmse"); boxplot(cv$res$pb, main="pb")

res = c(t(cv$res$val))

par(mfrow=c(1, 1), , mar=c(2, 2, 2, 2))
qqnorm(res); qqline(res)
hist(res, breaks = 50)
plot(res, type="l")

######### Separate model

eq.spow <- Solar_power ~ Nebulosity + Solar_power.1 + Solar_power.7 
eq.wpow <- Wind_power ~ Wind + Wind_power.1 + Wind_power.7

eq.load <- Load ~ s(toy, k=30, bs="cc") + s(Time, k=3, bs="cr") +
  WeekDays + BH_Holiday + BH + BH_before + DLS +
  s(Load.1, bs="cr") + Load.7 + 
  te(Temp, Wind, k=c(7, 3), bs="cr") + Temp_s95_max + Temp_s95_min + 
  s(Nebulosity, by=Year, bs="cr") +
  StringencyIndex_Average

#########################################
##### Calibration fenêtre d'apprentissage
#########################################

testset_size = nrow(Data1)
k = floor(nrow(Data0)/testset_size)
trainset_sizes = nrow(Data0) - c(1:k) * testset_size

f = Vectorize(function(trainset_size) time_cv(lm, eq, Data0, trainset_size, testset_size, type = "window")$res[c("pb", "rmse")], "trainset_size", SIMPLIFY = FALSE)

metrics = transpose(f(trainset_sizes))

par(mfrow=c(2,1))
boxplot(metrics$rmse, names=trainset_sizes, ylim=range(500, 2000))
boxplot(metrics$pb, names=trainset_sizes, ylim=range(100, 200))

########################################
##### ARIMA
########################################

mod.gam.pred = c(t(time_cv(gam, eq, Data0, trainset_size, testset_size, type = "window")$pred))
mod.gam.pred.ts <- ts(mod.gam.pred,  frequency= 7)
Net_demand.ts = ts(tail(Data0$Net_demand, k*testset_size), frequency=7)

mod.gam.arima <- auto.arima(Net_demand.ts, trace = T, xreg=mod.gam.pred.ts)

mod.gam <- gam(eq,  data=tail(Data0, trainset_size))
mod.gam.forecast = predict(mod.gam, Data1)
mod.gam.pred.1 <- lag(c(tail(mod.gam$fitted.values, testset_size+1), mod.gam.forecast))[-1]
mod.gam.forecast.1 = tail(mod.gam.pred.1, testset_size)

mod.gam.pred.1.ts <- ts(mod.gam.pred.1,  frequency= 7)
Net_demand.1.ts = ts(tail(c(Data0$Net_demand.1, Data1$Net_demand.1), 2*testset_size), frequency=7)

mod.gam.arima.1 = Arima(Net_demand.1.ts, xreg=mod.gam.pred.1.ts, model=mod.gam.arima)
checkresiduals(mod.gam.arima.1)

mod.gam.arima.forecast.1 = tail(mod.gam.arima.1$fitted, testset_size)
quant = tail(sliding_quantile(mod.gam.arima.1$residuals, window_size=testset_size, .95), testset_size+1)
quant.1 = lag(quant)[-1]
quant = quant[-1]

pinball_loss(Data1$Net_demand.1[-1], (mod.gam.arima.forecast.1 + quant.1)[-1], .95)

par(mfrow=c(1, 1))
plot(Data1$Date, Data1$Net_demand.1, type="l")
lines(Data1$Date, mod.gam.forecast.1, col="blue")
lines(Data1$Date, mod.gam.arima.forecast.1, col="red")

mod.gam.arima.forecast = lead(mod.gam.arima.forecast.1, 
                              default = forecast(mod.gam.arima.1, 
                                                 xreg = tail(mod.gam.pred.ts, 1))$mean)

submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Net_demand <- mod.gam.arima.forecast + quant
write.table(submit, file = "Data/submission_mod.csv", quote = F, sep = ",", dec = ".", row.names = F)
