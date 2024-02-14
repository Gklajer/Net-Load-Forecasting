rm(list = objects())
graphics.off()
library(tidyverse)
library(lubridate)
library(forecast)
source("Utils/score.R")
source("Utils/utils.R")


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

summary(Data0)


sel_a <- which(Data0$Year <= 2021)
sel_b <- which(Data0$Year > 2021)
sel_c <- which(Data0$Year >= 2018)

##### regroupement de modalités
Data0$WeekDaysGrouped <- forcats::fct_recode(weekdays(Data0$Date), "WorkDay" = "Thursday", "WorkDay" = "Tuesday", "WorkDay" = "Wednesday", "WorkDay" = "Friday")
Data1$WeekDaysGrouped <- forcats::fct_recode(weekdays(Data1$Date), "WorkDay" = "Thursday", "WorkDay" = "Tuesday", "WorkDay" = "Wednesday", "WorkDay" = "Friday")


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

#### Temperatures Spline Features
Data0$Temp_trunc1 <- pmax(Data0$Temp - 285, 0)
Data0$Temp_trunc2 <- pmax(Data0$Temp - 295, 0)

Data1$Temp_trunc1 <- pmax(Data1$Temp - 285, 0)
Data1$Temp_trunc2 <- pmax(Data1$Temp - 295, 0)

#### Fourier features

w <- 2 * pi / (365)
Nfourier <- 50
for (i in c(1:Nfourier))
{
  assign(paste("cos", i, sep = ""), cos(w * Data0$Time * i))
  assign(paste("sin", i, sep = ""), sin(w * Data0$Time * i))
}

cos <- paste("cos", c(1:Nfourier), sep = "", collapse = ",")
sin <- paste("sin", c(1:Nfourier), sep = "", collapse = ",")

Data0 <- eval(parse(text = paste("data.frame(Data0,", cos, ",", sin, ")", sep = "")))

Nfourier <- 10

cos <- paste(c("cos"), c(1:Nfourier), sep = "")
sin <- paste(c("sin"), c(1:Nfourier), sep = "")

Data0$Fourier10 <- eval(parse(text = paste(c(cos, sin), collapse = "+")))

for (i in c(1:Nfourier))
{
  assign(paste("cos", i, sep = ""), cos(w * Data1$Time * i))
  assign(paste("sin", i, sep = ""), sin(w * Data1$Time * i))
}

cos <- paste("cos", c(1:Nfourier), sep = "", collapse = ",")
sin <- paste("sin", c(1:Nfourier), sep = "", collapse = ",")

Data1 <- eval(parse(text = paste("data.frame(Data1,", cos, ",", sin, ")", sep = "")))

Nfourier <- 10

cos <- paste(c("cos"), c(1:Nfourier), sep = "")
sin <- paste(c("sin"), c(1:Nfourier), sep = "")

Data1$Fourier10 <- eval(parse(text = paste(c(cos, sin), collapse = "+")))

### Nébulosité
Data0$Nebulosity_transformed = Data0$Nebulosity_weighted
Data0$Nebulosity_transformed[-sel_c] = Data0$Nebulosity_transformed[-sel_c] * sqrt(var(Data0$Nebulosity_transformed[sel_c]) / var(Data0$Nebulosity_transformed[-sel_c]))
Data0$Nebulosity_transformed[-sel_c] = Data0$Nebulosity_transformed[-sel_c] + mean(Data0$Nebulosity_transformed[sel_c]) - mean(Data0$Nebulosity_transformed[-sel_c])

Data1$Nebulosity_transformed = Data1$Nebulosity_weighted
Data1$Nebulosity_transformed[-sel_c] = Data1$Nebulosity_transformed[-sel_c] * sqrt(var(Data1$Nebulosity_transformed[sel_c]) / var(Data1$Nebulosity_transformed[-sel_c]))
Data1$Nebulosity_transformed[-sel_c] = Data1$Nebulosity_transformed[-sel_c] + mean(Data1$Nebulosity_transformed[sel_c]) - mean(Data1$Nebulosity_transformed[-sel_c])

###############################################################################################################################################################
##################################################### feature engineering
###############################################################################################################################################################

################################################################################## Cycle hebdo
mod0 <- lm(Net_demand ~ WeekDays, data = Data0[sel_a, ])
summary(mod0)
mod0.forecast <- predict(mod0, newdata = Data0[sel_b, ])
rmse(y = Data0$Net_demand[sel_b], ychap = mod0.forecast)

mod0.cvpred <- lapply(block_list, fitmod, eq = "Net_demand ~ WeekDays") %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod0.cvpred, digits = 2)

res <- Data0$Net_demand - mod0.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb0 <- pinball_loss(y = Data0$Net_demand[sel_b], mod0.forecast + quant, quant = 0.95, output.vect = FALSE)
summary(Data0$WeekDaysGrouped)

mod0 <- lm(Net_demand ~ WeekDaysGrouped , data = Data0[sel_a, ])
summary(mod0)
mod0.forecast <- predict(mod0, newdata = Data0[sel_b, ])
rmse(y = Data0$Net_demand[sel_b], ychap = mod0.forecast)

mod0.cvpred <- lapply(block_list, fitmod, eq = "Net_demand ~ WeekDaysGrouped") %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod0.cvpred, digits = 2)

res <- Data0$Net_demand - mod0.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb0 <- pinball_loss(y = Data0$Net_demand[sel_b], mod0.forecast + quant, quant = 0.95, output.vect = FALSE)

#### Temperature polynomial/spline features

mod1 <- lm(Net_demand ~ WeekDaysGrouped + Temp, data = Data0[sel_a, ])
summary(mod1)
mod1.forecast <- predict(mod1, newdata = Data0[sel_b, ])
rmse(y = Data0$Net_demand[sel_b], ychap = mod1.forecast)
mod1.cvpred <- lapply(block_list, fitmod, eq = "Net_demand ~ WeekDaysGrouped + Temp") %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod1.cvpred)

res <- Data0$Net_demand - mod1.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb1 <- pinball_loss(y = Data0$Net_demand[sel_b], mod1.forecast + quant, quant = 0.95, output.vect = FALSE)

plot(Data0[sel_a, ]$Temp, Data0[sel_a, ]$Net_demand)
plot(Data0[sel_a, ]$Temp, mod1$residuals)

mod2 <- lm(Net_demand ~ WeekDaysGrouped + Temp + I(Temp^2), data = Data0[sel_a, ])
mod2.forecast <- predict(mod2, newdata = Data0[sel_b, ])
summary(mod2)
rmse(y = Data0$Net_demand[sel_b], ychap = mod2.forecast)

mod2.cvpred <- lapply(block_list, fitmod, eq = "Net_demand ~ WeekDaysGrouped + Temp +I(Temp^2)") %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod2.cvpred)

res <- Data0$Net_demand - mod2.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb2 <- pinball_loss(y = Data0$Net_demand[sel_b], mod2.forecast + quant, quant = 0.95, output.vect = FALSE)

plot(Data0$Temp, Data0$Net_demand, pch = 20)

plot(Data0$Temp, Data0$Temp_trunc1, pch = 20)

mod3 <- lm(Net_demand ~ WeekDaysGrouped + Temp + Temp_trunc1 + Temp_trunc2, data = Data0[sel_a, ])
mod3.forecast <- predict(mod3, newdata = Data0[sel_b, ])
summary(mod3)
rmse(y = Data0$Net_demand[sel_b], ychap = mod3.forecast)

mod3.cvpred <- lapply(block_list, fitmod, eq = "Net_demand ~ WeekDaysGrouped + Temp + Temp_trunc1 + Temp_trunc2") %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod3.cvpred)
res <- Data0$Net_demand - mod2.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb3 <- pinball_loss(y = Data0$Net_demand[sel_b], mod3.forecast + quant, quant = 0.95, output.vect = FALSE)

plot(Data0[sel_a, ]$Temp, mod3$residuals)

##

Nfourier <- 30
lm.fourier <- list()
eq <- list()
for (i in c(1:Nfourier))
{
  cos <- paste(c("cos"), c(1:i), sep = "")
  sin <- paste(c("sin"), c(1:i), sep = "")
  fourier <- paste(c(cos, sin), collapse = "+")
  eq[[i]] <- as.formula(paste("Net_demand~ WeekDaysGrouped + Temp + Temp_trunc1 + Temp_trunc2+", fourier, sep = ""))
  lm.fourier[[i]] <- lm(eq[[i]], data = Data0[sel_a, ])
}

lm(eq[[1]], data = Data0)


adj.rsquare <- lapply(
  lm.fourier,
  function(x) {
    summary(x)$adj.r.squared
  }
) %>% unlist()

fit.rmse <- lapply(
  lm.fourier,
  function(x) {
    rmse(Data0$Net_demand[sel_a], x$fitted)
  }
) %>% unlist()

forecast.rmse <- lapply(
  lm.fourier,
  function(x) {
    rmse(Data0$Net_demand[sel_b], predict(x, newdata = Data0[sel_b, ]))
  }
) %>% unlist()

fit.mape <- lapply(
  lm.fourier,
  function(x) {
    mape(Data0$Net_demand[sel_a], x$fitted)
  }
) %>% unlist()

forecast.mape <- lapply(
  lm.fourier,
  function(x) {
    mape(Data0$Net_demand[sel_b], predict(x, newdata = Data0[sel_b, ]))
  }
) %>% unlist()


plot(adj.rsquare, type = "b", pch = 20)

plot(fit.rmse, type = "b", pch = 20, ylim = range(fit.rmse, forecast.rmse), col = "royalblue2")
lines(forecast.rmse, type = "b", pch = 20, col = "orangered2")
legend("top", c("fit", "forecast"), col = c("royalblue2", "orangered2"), lty = 1)

##

eq4 <- as.formula("Net_demand ~ WeekDaysGrouped + Temp + Temp_trunc1 + Temp_trunc2 + Fourier10")

mod4 <- lm(eq4, data = Data0[sel_a, ])
mod4.forecast <- predict(mod4, newdata = Data0[sel_b, ])
summary(mod4)
rmse(y = Data0$Net_demand[sel_b], ychap = mod4.forecast)

mod4.cvpred <- lapply(block_list, fitmod, eq = eq4) %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod4.cvpred)

res <- Data0$Net_demand - mod4.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb4 <- pinball_loss(y = Data0$Net_demand[sel_b], mod4.forecast + quant, quant = 0.95, output.vect = FALSE)



######### Additional features
eq5 <- as.formula("Net_demand ~ WeekDays + Net_demand.1 + Net_demand.7 + Temp + Temp_trunc1 + Temp_trunc2 + Nebulosity + Wind + Wind_power.1 + Solar_power.1 + BH + BH_before + Holiday + StringencyIndex_Average + Fourier10")

mod5 <- lm(eq5, data = Data0[sel_c, ])
mod5.forecast <- predict(mod5)
mod5.forecast <- predict(mod5, newdata = Data1)
summary(mod5)
rmse(y = Data0$Net_demand[sel_b], ychap = mod5.forecast)

mod5.cvpred <- lapply(block_list, fitmod, eq = eq5) %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod5.cvpred)

res <- Data0$Net_demand[sel_c] - mod5.forecast
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb5 <- pinball_loss(y = Data0$Net_demand[sel_b], mod5.forecast + quant, quant = 0.95, output.vect = FALSE)

submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Net_demand <- mod5.forecast + quant
write.table(submit, file = "Data/submission_lm.csv", quote = F, sep = ",", dec = ".", row.names = F)


########################################
##### Quantile regression
########################################
library("quantreg")

mod5.rq <- rq(eq5, data = Data0[sel_c, ], tau = 0.95)
summary(mod5.rq)

mod5.rq.forecast <- predict(mod5.rq, newdata = Data0[sel_b, ])
pb_rq <- pinball_loss(y = Data0$Net_demand[sel_b], mod5.rq.forecast, quant = 0.95, output.vect = FALSE)

mod5.rq.pred <- predict(mod5.rq, newdata = Data1)

submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Net_demand <- mod5.rq.pred
write.table(submit, file = "Data/submission_rq.csv", quote = F, sep = ",", dec = ".", row.names = F)


##############
############

par(mfrow=c(3, 1))
plot(Data0$Date, Data0$Nebulosity, type = 'l')
plot(Data0$Date, Data0$Nebulosity_weighted, type = 'l', col="red")
plot(Data0$Date, Data0$Nebulosity_transformed, type = 'l', col="darkred")

#############
par(mfrow=c(1, 1))

res = Data0$Net_demand[sel_c]-mod5.forecast
plot(Data0$Nebulosity[sel_c], Data0$Net_demand[sel_c]-mod5.forecast)

plot(Data0$Nebulosity[sel_c], Data0$Solar_power[sel_c])

plot(Data0$toy, Data0$Solar_power)


plot(Data0$Wind_weighted, Data0$Wind_power)
plot(Data0$Wind_power.7, Data0$Wind_power)
plot(Data0$, Data0$Wind_power)
  
################################## analyse de la demande nette

############################################ trend
boxplot(Net_demand ~ BH, Data0)

plot(head(Data0$Net_demand.7), head(Data0$Net_demand))
plot(Data0$Date, Data0$Temp, type = "l", xlim = range(Data0$Date, Data1$Date))
lines(Data0$Date, Data0$Temp_s95, col="blue")
start = as.numeric(unlist((str_split(date(Data0$Date[1]), "-"))))
y <- ts(Data0$Net_demand, start = start, frequency = 365)

autoplot(y)

autoplot(diff(diff(diff(y, lag = 365), lag = 7)))
fourier(y, K=5)
acf(diff(y))
fit <- tslm(y ~ trend + season)
fit$coefficients
plot(c(Data0$Date, Data1$Date), forecast(fit, h=length(Data1)))
fit <- tslm(y ~ trend + season)
plot(forecast(fit, h=20))
trend <- lm(Net_demand ~ Time, data = Data0)
abline(mod0$coefficients, col="blue")

Data0$Net_demand_detrended = Data0$Net_demand - trend$fitted.values
plot(Data0$Date, Data0$Net_demand_detrended, type = "l", xlim = range(Data0$Date, Data1$Date))

fft_out <- fft(Data0$Net_demand_detrended)
spectre = Mod(fft_out[2 : (length(fft_out)/2 + 1)])
barplot(spectre)

spectre_sorted = sort(spectre, decreasing = TRUE)
cumsum(spectre_sorted)[400] / sum(spectre_sorted) * 100
