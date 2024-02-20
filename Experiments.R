rm(list = objects()); graphics.off()

library(lubridate)
library(forecast)
library(magrittr)
library(mgcv)
library(quantreg)
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
Data1$Nebulosity_transformed[-sel_c] = Data1$Nebulosity_transformed[-sel_c] * sqrt(var(Data1$Nebulosity_transformed[sel_c]) / var(Data1$Nebulosity_transformed[-sel_c]))
Data1$Nebulosity_transformed[-sel_c] = Data1$Nebulosity_transformed[-sel_c] + mean(Data1$Nebulosity_transformed[sel_c]) - mean(Data1$Nebulosity_transformed[-sel_c])

###### bloc CV
Nblock <- 8
borne_block <- seq(1, nrow(Data0), length = Nblock + 1) %>% floor()
block_list <- list()
l <- length(borne_block)
for (i in c(2:(l - 1)))
{
  block_list[[i - 1]] <- c(borne_block[i - 1]:(borne_block[i] - 1))
}
block_list[[l - 1]] <- c(borne_block[l - 1]:(borne_block[l]))
###############################################################################################################################################################

########################################
##### Linear Models
########################################

eq <- Net_demand ~ WeekDays + Temp + Temp_trunc1 + Temp_trunc2 + Fourier10

mod.lm <- lm(eq, data = Data0[sel_a,])
summary(mod.lm)

nb_eval = 50
trainset_size = nrow(Data0) - nrow(Data1) - (nb_eval - 1)
testset_size = nrow(Data1)

cv1 = time_cv(lm, eq, Data0, trainset_size, testset_size, nb_eval, type = "rolling")

cv = cv1

par(mfrow=c(2, 2))
boxplot(cv$res$mean, ylab="in MW", main="mean"); boxplot(cv$res$quant, main="quant")
boxplot(cv$res$rmse, ylab="in MW", main="rmse"); boxplot(cv$res$pb, main="pb")

med_pb_idx = which.median(cv$res$pb)
res_med_pb = as.numeric(cv$res$val[med_pb_idx,])

par(mfrow=c(1, 1))
qqnorm(res_med_pb); qqline(res_med_pb)
hist(res_med_pb, breaks = 50)
plot(res_med_pb, type="l")

######### Additional features

eq <- Net_demand ~ WeekDays + Net_demand.1 + Net_demand.7 + Temp + Temp_trunc1 +
  Temp_trunc2 + Nebulosity + Wind + Wind_power.1 + Solar_power.1 +
  BH + BH_before + Holiday + StringencyIndex_Average + Fourier10

mod.lm <- lm(eq, data = Data0[sel_a,])
summary(mod.lm)

cv2 = time_cv(lm, eq, Data0, trainset_size, testset_size, nb_eval, type = "rolling")

cv = cv2

par(mfrow=c(2, 2))
boxplot(cv$res$mean, ylab="in MW", main="mean"); boxplot(cv$res$quant, main="quant")
boxplot(cv$res$rmse, ylab="in MW", main="rmse"); boxplot(cv$res$pb, main="pb")

med_pb_idx = which.median(cv$res$pb)
res_med_pb = as.numeric(cv$res$val[med_pb_idx,])

par(mfrow=c(1, 1))
qqnorm(res_med_pb); qqline(res_med_pb)
hist(res_med_pb, breaks = 50)
plot(res_med_pb, type="l")


########################################
##### Trainset size calibration
########################################

nb_eval = 50
testset_size = nrow(Data1)
max_window = nrow(Data0) - testset_size - (nb_eval - 1)
trainset_sizes = seq(200, max_window, length.out=40) %>% floor()
f = Vectorize(function(trainset_size) time_cv(lm, eq, Data0, trainset_size, testset_size, nb_eval, type = "window")$res$rmse, "trainset_size")
rmse_df = as.data.frame(f(trainset_sizes))
colnames(rmse_df) <- trainset_sizes

par(mfrow=c(1, 1))
boxplot(rmse_df)
boxplot(rmse_df[,1:31])
boxplot(rmse_df[,1:15], ylim=range(1000, 2000))


max_window = 1000
trainset_sizes = seq(365, max_window, length.out=40) %>% floor()
f = Vectorize(function(trainset_size) time_cv(lm, eq, Data0, trainset_size, testset_size, nb_eval, type = "window")$res$rmse, "trainset_size")
rmse_df = as.data.frame(f(trainset_sizes))
colnames(rmse_df) <- trainset_sizes

par(mfrow=c(1, 1))
boxplot(rmse_df, ylim=range(1000, 2000))

########################################
##### Quantile regression
########################################

mod.rq <- rq(eq, data = Data0[sel_a,])
summary(mod.rq)

cv = time_cv(rq, eq, Data0, trainset_size, testset_size, nb_eval, type = "rolling", is.rq = T, tau = .95)

par(mfrow=c(1, 1))
boxplot(cv$res$pb, ylab="in MW", main="pb")

create_submission(rq, eq, tau = 0.95)

########################################
##### GAMs
########################################

eq <- Net_demand ~ s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') +
  WeekDays + BH +
  s(Net_demand.1, bs='cr') +  s(Net_demand.7, bs='cr') +
  s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp,k=10, bs='cr') + s(Temp_s99,k=10, bs='cr') + 
  s(Wind) + te(as.numeric(Date), Nebulosity, k=c(4,10))
  

mod.gam <- gam(eq,  data=Data0[sel_a,])
summary(mod.gam)

nb_eval = 20
trainset_size = nrow(Data0) - nrow(Data1) - (nb_eval - 1)
testset_size = nrow(Data1)

cv3 = time_cv(gam, eq, Data0, trainset_size, testset_size, nb_eval, type = "rolling")

cv = cv3

par(mfrow=c(2, 2))
boxplot(cv$res$mean, ylab="in MW", main="mean"); boxplot(cv$res$quant, main="quant")
boxplot(cv$res$rmse, ylab="in MW", main="rmse"); boxplot(cv$res$pb, main="pb")

med_pb_idx = which.median(cv$res$pb)
res_med_pb = as.numeric(cv$res$val[med_pb_idx,])

par(mfrow=c(1, 1))
qqnorm(res_med_pb); qqline(res_med_pb)
hist(res_med_pb, breaks = 50)
plot(res_med_pb, type="l")

######### Additional features
eq <- Net_demand ~ s(as.numeric(Date), k=3, bs='cr') + s(toy,k=30, bs='cc') +
  WeekDays + BH + BH_before + Holiday + 
  s(Net_demand.1, bs='cr') + s(Net_demand.7, k=10, bs='cr') + 
  s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Wind_power.1, k=5, bs="cr") + s(Solar_power.1, bs='cr') +
  s(Temp,k=10, bs='cr') + s(Temp_s99,k=10, bs='cr') +
  s(Wind, bs='cr') + te(as.numeric(Date), Nebulosity, k=c(4,10))

mod.gam <- gam(eq,  data=Data0[sel_a,])
summary(mod.gam)

trainset_size = 1500
cv4 = time_cv(gam, eq, Data0, trainset_size, testset_size, nb_eval, type = "window")

cv = cv4

par(mfrow=c(2, 2))
boxplot(cv$res$mean, ylab="in MW", main="mean"); boxplot(cv$res$quant, main="quant")
boxplot(cv$res$rmse, ylab="in MW", main="rmse"); boxplot(cv$res$pb, main="pb")

med_pb_idx = which.median(cv$res$pb)
res_med_pb = as.numeric(cv$res$val[med_pb_idx,])

par(mfrow=c(1, 1))
qqnorm(res_med_pb); qqline(res_med_pb)
hist(res_med_pb, breaks = 50)
plot(res_med_pb, type="l")

create_submission(gam, eq, train_data = tail(Data0, trainset_size), quant = min(cv$res$quant))

########################################
##### ARIMA
########################################

block_res = lapply(block_list, function(idx_test) train_eval(gam, eq, Data0[-idx_test,], Data0[idx_test,])$res) %>% unlist()
block_res.ts <- ts(block_res, frequency=7)

mod.res.arima <- auto.arima(block_res.ts,max.p=3,max.q=4, max.P=2, max.Q=2, trace=T,ic="aic", method="CSS")
#Best model: ARIMA(3,0,4)(1,0,0)[7] with zero mean   
#saveRDS(fit.arima.res, "Results/tif.arima.res.RDS")

mod.gam.forecast = predict(mod.gam, newdata=Data0[sel_b,])
ts_res_forecast <- ts(c(block_res.ts, Data0[sel_b,]$Net_demand-mod.gam.forecast),  frequency= 7)

mod.res.arima <- Arima(ts_res_forecast, model=mod.res.arima)
summary(mod.res.arima)

rmse.old(mod.res.arima$residuals)

mod.res.arima.forecast <- tail(mod.res.arima$fitted, nrow(Data0[sel_b,]))

mod.gam_arima.forecast <- mod.gam.forecast + mod.res.arima.forecast + 

rmse.old(Data0[sel_b,]$Net_demand-mod.gam.forecast)
rmse.old(Data0[sel_b,]$Net_demand-mod.gam_arima.forecast)
mape(Data0[sel_b,]$Net_demand, mod.gam_arima.forecast)