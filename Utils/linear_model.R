rm(list = objects())
library(tidyverse)
library(lubridate)
source("Utils/score.R")

Data0 <- read_delim("Data/train.csv", delim = ",")
Data1 <- read_delim("Data/test.csv", delim = ",")

range(Data0$Date)

Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)


sel_a <- which(Data0$Year <= 2021)
sel_b <- which(Data0$Year > 2021)



###############################################################################################################################################################
##################################################### feature engineering
###############################################################################################################################################################

################################################################################## Cycle hebdo
Data0$WeekDays <- as.factor(Data0$WeekDays)
Data1$WeekDays <- as.factor(Data1$WeekDays)


mod0 <- lm(Net_demand ~ WeekDays, data = Data0[sel_a, ])
summary(mod0)
mod0.forecast <- predict(mod0, newdata = Data0[sel_b, ])
rmse(y = Data0$Net_demand[sel_b], ychap = mod0.forecast)


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

rmse <- function(y, ychap, digits = 0) {
  return(round(sqrt(mean((y - ychap)^2, na.rm = TRUE)), digits = digits))
}


fitmod <- function(eq, block) {
  mod <- lm(eq, data = Data0[-block, ])
  mod.cvpred <- predict(mod, newdata = Data0[block, ])
  return(mod.cvpred)
}

mod0.cvpred <- lapply(block_list, fitmod, eq = "Net_demand ~ WeekDays") %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod0.cvpred, digits = 2)

res <- Data0$Net_demand - mod0.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pinball_loss(y = Data0$Net_demand[sel_b], mod0.forecast + quant, quant = 0.95, output.vect = FALSE)




##### regroupement de modalités

Data0$WeekDays2 <- weekdays(Data0$Date)
Data0$WeekDays3 <- forcats::fct_recode(Data0$WeekDays2, "WorkDay" = "Thursday", "WorkDay" = "Tuesday", "WorkDay" = "Wednesday")

summary(Data0$WeekDays3)


mod0 <- lm(Net_demand ~ WeekDays3, data = Data0[sel_a, ])
summary(mod0)
mod0.forecast <- predict(mod0, newdata = Data0[sel_b, ])
rmse(y = Data0$Net_demand[sel_b], ychap = mod0.forecast)

mod0.cvpred <- lapply(block_list, fitmod, eq = "Net_demand ~ WeekDays3") %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod0.cvpred, digits = 2)


res <- Data0$Net_demand - mod0.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb0 <- pinball_loss(y = Data0$Net_demand[sel_b], mod0.forecast + quant, quant = 0.95, output.vect = FALSE)



################################################################################################################################################################
################################################################################## Temperature
################################################################################################################################################################

############################################### polynomial transforms
mod1 <- lm(Net_demand ~ WeekDays3 + Temp, data = Data0[sel_a, ])
summary(mod1)
mod1.forecast <- predict(mod1, newdata = Data0[sel_b, ])
rmse(y = Data0$Net_demand[sel_b], ychap = mod1.forecast)
mod1.cvpred <- lapply(block_list, fitmod, eq = "Net_demand ~ WeekDays3 + Temp") %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod1.cvpred)

res <- Data0$Net_demand - mod1.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb1 <- pinball_loss(y = Data0$Net_demand[sel_b], mod1.forecast + quant, quant = 0.95, output.vect = FALSE)



plot(Data0[sel_a, ]$Temp, Data0[sel_a, ]$Net_demand)
plot(Data0[sel_a, ]$Temp, mod1$residuals)


mod2 <- lm(Net_demand ~ WeekDays3 + Temp + I(Temp^2), data = Data0[sel_a, ])
mod2.forecast <- predict(mod2, newdata = Data0[sel_b, ])
summary(mod2)
rmse(y = Data0$Net_demand[sel_b], ychap = mod2.forecast)

mod2.cvpred <- lapply(block_list, fitmod, eq = "Net_demand ~ WeekDays3 + Temp +I(Temp^2)") %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod2.cvpred)


res <- Data0$Net_demand - mod2.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb2 <- pinball_loss(y = Data0$Net_demand[sel_b], mod2.forecast + quant, quant = 0.95, output.vect = FALSE)



plot(Data0[sel_a, ]$Temp, mod2$residuals)

plot(Data0$Date, Data0$Net_demand - mod2.cvpred, type = "l")
lines(Data0$Date[sel_b], Data0$Net_demand[sel_b] - mod2.forecast, col = "red")


## variance des scores par bloc?
mod1.rmse_bloc <- lapply(block_list, function(x) {
  rmse(y = Data0$Net_demand[x], ychap = mod1.cvpred[x])
}) %>% unlist()
mod2.rmse_bloc <- lapply(block_list, function(x) {
  rmse(y = Data0$Net_demand[x], ychap = mod2.cvpred[x])
}) %>% unlist()

col <- yarrr::piratepal("basel")
boxplot(cbind(mod1.rmse_bloc, mod2.rmse_bloc), col = col[1:2], ylim = c(2000, 10000))
abline(h = rmse(y = Data0$Load[sel_b], ychap = mod1.forecast), col = col[1], lty = "dotted")
abline(h = rmse(y = Data0$Load[sel_b], ychap = mod2.forecast), col = col[2], lty = "dotted")




############################################### truncated power functions
# for(i in c(1:11))
# {
#   x<-pmax(eval(parse(text=paste0("Data0$Station",i)))-65,0)
#   assign(paste("Station",i,".trunc.65",sep=""), x)
# }

plot(Data0$Temp, Data0$Net_demand, pch = 20)
Data0$Temp_trunc1 <- pmax(Data0$Temp - 285, 0)
Data0$Temp_trunc2 <- pmax(Data0$Temp - 295, 0)

plot(Data0$Temp, Data0$Temp_trunc1, pch = 20)


mod3 <- lm(Net_demand ~ WeekDays3 + Temp + Temp_trunc1 + Temp_trunc2, data = Data0[sel_a, ])
mod3.forecast <- predict(mod3, newdata = Data0[sel_b, ])
summary(mod3)
rmse(y = Data0$Net_demand[sel_b], ychap = mod3.forecast)


mod3.cvpred <- lapply(block_list, fitmod, eq = "Net_demand ~ WeekDays3 + Temp + Temp_trunc1 + Temp_trunc2") %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod3.cvpred)
res <- Data0$Net_demand - mod2.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb3 <- pinball_loss(y = Data0$Net_demand[sel_b], mod3.forecast + quant, quant = 0.95, output.vect = FALSE)


mod3.rmse_bloc <- lapply(block_list, function(x) {
  rmse(y = Data0$Net_demand[x], ychap = mod3.cvpred[x])
}) %>% unlist()

col <- yarrr::piratepal("basel")
boxplot(cbind(mod2.rmse_bloc, mod3.rmse_bloc), col = col[1:2], ylim = c(2000, 7000))
abline(h = rmse(y = Data0$Net_demand[sel_b], ychap = mod2.forecast), col = col[1], lty = "dotted")
abline(h = rmse(y = Data0$Net_demand[sel_b], ychap = mod3.forecast), col = col[2], lty = "dotted")

plot(Data0$Date[sel_b], mod3.cvpred[sel_b], type = "l")
lines(Data0$Date[sel_b], mod3.forecast, col = "red")

plot(Data0[sel_a, ]$Temp, mod2$residuals)
points(Data0[sel_a, ]$Temp, mod3$residuals, col = "red")

plot(Data0$Date, Data0$Net_demand - mod3.cvpred, type = "l")



################################################################################## cycle annuel: fourier
w <- 2 * pi / (365)
Nfourier <- 50
for (i in c(1:Nfourier))
{
  assign(paste("cos", i, sep = ""), cos(w * Data0$Time * i))
  assign(paste("sin", i, sep = ""), sin(w * Data0$Time * i))
}
objects()
plot(Data0$Date, cos1, type = "l")

cos <- paste("cos", c(1:Nfourier), sep = "", collapse = ",")
sin <- paste("sin", c(1:Nfourier), sep = "", collapse = ",")

Data0 <- eval(parse(text = paste("data.frame(Data0,", cos, ",", sin, ")", sep = "")))
names(Data0)




Nfourier <- 30
lm.fourier <- list()
eq <- list()
for (i in c(1:Nfourier))
{
  cos <- paste(c("cos"), c(1:i), sep = "")
  sin <- paste(c("sin"), c(1:i), sep = "")
  fourier <- paste(c(cos, sin), collapse = "+")
  eq[[i]] <- as.formula(paste("Net_demand~ WeekDays3 + Temp + Temp_trunc1 + Temp_trunc2+", fourier, sep = ""))
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


mod4 <- lm(formula(lm.fourier[[10]]), data = Data0[sel_a, ])
mod4.cvpred <- lapply(block_list, fitmod, eq = formula(lm.fourier[[15]])) %>% unlist()
mod4.forecast <- predict(mod4, newdata = Data0[sel_b, ])
rmse(y = Data0$Net_demand, ychap = mod4.cvpred)
rmse(y = Data0$Net_demand[sel_b], ychap = mod4.forecast)
mod4.rmse_bloc <- lapply(block_list, function(x) {
  rmse(y = Data0$Net_demand[x], ychap = mod4.cvpred[x])
}) %>% unlist()

res <- Data0$Net_demand - mod4.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb4 <- pinball_loss(y = Data0$Net_demand[sel_b], mod4.forecast + quant, quant = 0.95, output.vect = FALSE)
pb4

plot(Data0$Date[sel_a], mod4$residuals, type = "l")
acf(Data0$Net_demand, lag.max = 7 * 3)

acf(mod4$residuals, lag.max = 7 * 3)


form <- eq[[10]]
form <- buildmer::add.terms(form, "Net_demand.1")
form <- buildmer::add.terms(form, "Net_demand.7")


mod5 <- lm(form, data = Data0[sel_a, ])
mod5.forecast <- predict(mod5, newdata = Data0[sel_b, ])
summary(mod5)

rmse(y = Data0$Net_demand[sel_b], ychap = mod4.forecast)
rmse(y = Data0$Net_demand[sel_b], ychap = mod5.forecast)

mod5.cvpred <- lapply(block_list, fitmod, eq = form) %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod5.cvpred)


res <- Data0$Net_demand - mod5.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))
pb5 <- pinball_loss(y = Data0$Net_demand[sel_b], mod5.forecast + quant, quant = 0.95, output.vect = FALSE)




synthese.test <- c(
  rmse(y = Data0$Net_demand[sel_b], ychap = mod1.forecast),
  rmse(y = Data0$Net_demand[sel_b], , ychap = mod2.forecast),
  rmse(y = Data0$Net_demand[sel_b], , ychap = mod3.forecast),
  rmse(y = Data0$Net_demand[sel_b], , ychap = mod4.forecast),
  rmse(y = Data0$Net_demand[sel_b], , ychap = mod5.forecast)
)

synthese.cv <- c(
  rmse(y = Data0$Net_demand, ychap = mod1.cvpred),
  rmse(y = Data0$Net_demand, ychap = mod2.cvpred),
  rmse(y = Data0$Net_demand, ychap = mod3.cvpred),
  rmse(y = Data0$Net_demand, ychap = mod4.cvpred),
  rmse(y = Data0$Net_demand, ychap = mod5.cvpred)
)


plot(synthese.test, type = "b", pch = 20, ylim = c(1000, 7000))
lines(synthese.cv, col = "red", pch = 20, type = "b")


synthese.pb <- c(pb0, pb1, pb2, pb3, pb4, pb5)
plot(synthese.pb, type = "b", pch = 20)


###########################################################################################
############# soumission d'une prévision
###########################################################################################
Data1$WeekDays2 <- weekdays(Data1$Date)
Data1$WeekDays3 <- forcats::fct_recode(Data1$WeekDays2, "WorkDay" = "Thursday", "WorkDay" = "Tuesday", "WorkDay" = "Wednesday")

Data1$Temp_trunc1 <- pmax(Data1$Temp - 285, 0)
Data1$Temp_trunc2 <- pmax(Data1$Temp - 290, 0)



################################################################################## cycle annuel: fourier
w <- 2 * pi / (365)
Nfourier <- 50
for (i in c(1:Nfourier))
{
  assign(paste("cos", i, sep = ""), cos(w * Data1$Time * i))
  assign(paste("sin", i, sep = ""), sin(w * Data1$Time * i))
}
objects()
plot(Data1$Date, cos1, type = "l")

cos <- paste("cos", c(1:Nfourier), sep = "", collapse = ",")
sin <- paste("sin", c(1:Nfourier), sep = "", collapse = ",")

Data1 <- eval(parse(text = paste("data.frame(Data1,", cos, ",", sin, ")", sep = "")))
names(Data1)





mod5final <- lm(form, data = Data0)

### prev moyenne
lm.forecast <- predict(mod5final, newdata = Data1)
### prev proba
res <- Data0$Net_demand - mod5.cvpred
quant <- qnorm(0.95, mean = mean(res), sd = sd(res))

pinball_loss(y = Data0$Net_demand[sel_b], mod5.forecast + quant, quant = 0.95, output.vect = FALSE)


submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Net_demand <- lm.forecast + quant
write.table(submit, file = "Data/submission_lm.csv", quote = F, sep = ",", dec = ".", row.names = F)



########################################
##### glm
########################################
res <- Data0$Net_demand - mod5.cvpred
hist(res, breaks = 50)
plot(res, type = "l")
eq <- form
eq_var <- ~ WeekDays3 + Temp + Temp_trunc1 + Temp_trunc2

glss <- mgcv::gam(list(eq, eq_var), data = Data0[sel_a, ], family = "gaulss")
summary(glss)
glss.forecast <- predict(glss, newdata = Data0[sel_b, ])
sigma <- 0.01 + exp(glss.forecast[, 2])
gaulss_quant <- glss.forecast[, 1] + qnorm(p = 0.95, mean = 0, sd = sigma)


pb_glm <- pinball_loss(y = Data0$Net_demand[sel_b], gaulss_quant, quant = 0.95, output.vect = FALSE)








########################################
##### Quantile regression
########################################
library("quantreg")


mod5.rq <- rq(form, data = Data0[sel_a, ], tau = 0.95)
summary(mod5.rq)

mod5.rq.forecast <- predict(mod5.rq, newdata = Data0[sel_b, ])
pb_rq <- pinball_loss(y = Data0$Net_demand[sel_b], mod5.rq.forecast, quant = 0.95, output.vect = FALSE)




synthese.pb <- c(pb3, pb4, pb5, pb_glm, pb_rq)
plot(synthese.pb, type = "b", pch = 20)


######################################################################################################################################################
############################## Annexes
#####################################################################################################################################################

########################################
##### Méthode d'ensemble
########################################
mod5 <- lm(form, data = Data0[sel_a, ])
mod5.forecast <- predict(mod5, newdata = Data0[sel_b, ])
summary(mod5)
rmse(y = Data0$Net_demand[sel_b], ychap = mod5.forecast)

mod5.cvpred <- lapply(block_list, fitmod, eq = form) %>% unlist()


fit.ensemble <- function(eq, block) {
  mod <- lm(eq, data = Data0[-block, ])
  mod.forecast <- predict(mod, newdata = Data1)
  return(mod.forecast)
}

mod5.ensemble <- lapply(block_list, fit.ensemble, eq = form)

mod5.ensemble <- mod5.ensemble %>%
  unlist() %>%
  matrix(ncol = length(block_list), nrow = nrow(Data1), byrow = F)
mod5.ensemble %>% head()
mod5.ensemble <- rowMeans(mod5.ensemble)




submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Net_demand <- mod5.ensemble
write.table(submit, file = "Data/submission_lm_ensemble_block.csv", quote = F, sep = ",", dec = ".", row.names = F)



###### random CV

fit.ensemble.random <- function(eq, block) {
  mod <- lm(eq, data = Data0[block, ])
  mod.forecast <- predict(mod, newdata = Data1)
  return(mod.forecast)
}

n <- nrow(Data0)
block2 <- lapply(rep(0.5, 100), function(rate) {
  sample(c(1:n), size = floor(rate * n), replace = T)
})
mod5.ensemble.random <- lapply(block2, fit.ensemble.random, eq = form)

mod5.ensemble.random <- mod5.ensemble.random %>%
  unlist() %>%
  matrix(ncol = length(block2), nrow = nrow(Data1), byrow = F)
mod5.ensemble.random %>% head()
mod5.ensemble.mean <- rowMeans(mod5.ensemble.random)

matplot(mod5.ensemble.random, type = "l", col = "gray")
lines(mod5.ensemble.mean)


fit.ensemble.random2 <- function(eq, block, Data) {
  mod <- lm(eq, data = Data[block, ])
  return(mod)
}

n <- nrow(Data0[sel_a, ])
block2 <- lapply(rep(0.9, 100), function(rate) {
  sample(c(1:n), size = floor(rate * n), replace = T)
})
mod.ensemble <- lapply(block2, fit.ensemble.random2, Data = Data0[sel_a, ], eq = form)
mod.ensemble.test <- lapply(mod.ensemble, predict, newdata = Data0[sel_b, ])
mod.ensemble.test <- mod.ensemble.test %>%
  unlist() %>%
  matrix(ncol = length(block2), nrow = nrow(Data0[sel_b, ]), byrow = F)
rmse(y = Data0$Net_demand[sel_b], ychap = rowMeans(mod.ensemble.test))

rmse.random <- apply(mod.ensemble.test, 2, rmse, y = Data0$Net_demand[sel_b])
boxplot(rmse.random)
abline(h = rmse(y = Data0$Net_demand[sel_b], ychap = rowMeans(mod.ensemble.test)), col = "red")

plot(synthese.test, type = "b", pch = 20, ylim = c(1000, 7000))
lines(synthese.cv, col = "red", pch = 20, type = "b")
abline(h = rmse(y = Data0$Net_demand[sel_b], ychap = rowMeans(mod.ensemble.test)), col = "red")




submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Net_demand <- mod5.ensemble.mean
write.table(submit, file = "Data/submission_lm_ensemble_random.csv", quote = F, sep = ",", dec = ".", row.names = F)


########################################
##### Quantile regression
########################################
library("quantreg")

fitmod.rq <- function(eq, block, tau = 0.5) {
  mod <- rq(eq, data = Data0[-block, ], tau)
  mod.cvpred <- predict(mod, newdata = Data0[block, ])
  return(mod.cvpred)
}

mod5.rq <- rq(form, data = Data0[sel_a, ], tau = 0.5)
summary(mod5.rq)

mod5.rq.forecast <- predict(mod5.rq, newdata = Data0[sel_b, ])

rmse(y = Data0$Net_demand[sel_b], ychap = mod5.rq.forecast)

mod5.rq.cvpred <- lapply(block_list, fitmod.rq, eq = form) %>% unlist()
rmse(y = Data0$Net_demand, ychap = mod5.rq.cvpred)
