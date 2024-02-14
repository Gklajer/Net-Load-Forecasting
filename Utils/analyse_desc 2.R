rm(list = objects())
library(tidyverse)
library(lubridate)
library(forecast)


Data0 <- read_delim("Data/train.csv", delim = ",")
Data1 <- read_delim("Data/test.csv", delim = ",")

summary(Data0)

range(Data0$Date)
range(Data1$Date)

names(Data0)
head(Data0[, c("Date", "WeekDays")])

weekdays(head(Data0$Date, 7))

# 5: Saturday
# 6: Sunday
# 0: Monday
# ...

################################### analyse de la demande nette

############################################ trend
plot(Data0$Date, Data0$Net_demand, type = "l", xlim = range(Data0$Date, Data1$Date))

col <- yarrr::piratepal("basel")
par(mfrow = c(3, 1))
plot(Data0$Date, Data0$Load, type = "l", col = col[1])
plot(Data0$Date, Data0$Solar_power, type = "l", col = col[2])
plot(Data0$Date, Data0$Wind_power, type = "l", col = col[3])

par(mfrow = c(1, 1))
plot(Data0$Date, Data0$Load, type = "l", ylim = range(Data0$Solar_power, Data0$Load), col = col[1])
lines(Data0$Date, Data0$Wind_power, col = col[3])
lines(Data0$Date, Data0$Solar_power, col = col[2])


hist(Data0$Net_demand, breaks = 100)

# plot(Data0$Date, Data0$Temp, type='l')

plot(Data0$Date, Data0$Net_demand, type = "l", xlim = range(Data0$Date, Data1$Date))
K <- 7 * 52
smooth <- stats::filter(Data0$Net_demand, rep(1 / K, K))
lines(Data0$Date, smooth, col = "red", lwd = 2)


############################################ yearly cycle
sel <- which(Data0$Year == 2021)
plot(Data0$Date[sel], Data0$Net_demand[sel], type = "l")

plot(Data0$toy)

col.tr <- adjustcolor(col = "black", alpha = 0.3)
plot(Data0$toy, Data0$Net_demand, pch = 16, col = col.tr)


col.tr <- adjustcolor(col, alpha = 0.3)
par(mfrow = c(3, 1))
plot(Data0$toy, Data0$Load, pch = 16, col = col.tr[1])
plot(Data0$toy, Data0$Solar_power, pch = 16, col = col.tr[2])
plot(Data0$toy, Data0$Wind_power, pch = 16, col = col.tr[3])

par(mfrow = c(3, 1))
boxplot(Net_demand ~ Month, data = Data0, col = col[1])
boxplot(Solar_power ~ Month, data = Data0, col = col[2])
boxplot(Wind_power ~ Month, data = Data0, col = col[3])


############################################ Weekly cycle
par(mfrow = c(1, 1))

sel <- which(Data0$Month == 6 & Data0$Year == 2021)
plot(Data0$Date[sel], Data0$Net_demand[sel], type = "l")

par(mfrow = c(3, 1))
boxplot(Net_demand ~ WeekDays, data = Data0, col = col[1])
boxplot(Solar_power ~ WeekDays, data = Data0, col = col[2])
boxplot(Wind_power ~ WeekDays, data = Data0, col = col[3])

par(mfrow = c(1, 1))
boxplot(Net_demand ~ WeekDays, data = Data0)



par(mfrow = c(1, 3))
Acf(Data0$Load, lag.max = 7 * 10, type = c("correlation"), col = col[1], ylim = c(0, 1))
Acf(Data0$Solar_power, lag.max = 7 * 10, type = c("correlation"), col = col[2], ylim = c(0, 1))
Acf(Data0$Wind_power, lag.max = 7 * 10, type = c("correlation"), col = col[3], ylim = c(0, 1))


par(mfrow = c(1, 3))
Acf(Data0$Load, lag.max = 7 * 60, type = c("correlation"), col = col[1], ylim = c(-1, 1))
Acf(Data0$Solar_power, lag.max = 7 * 60, type = c("correlation"), col = col[2], ylim = c(-1, 1))
Acf(Data0$Wind_power, lag.max = 7 * 60, type = c("correlation"), col = col[3], ylim = c(-1, 1))


####################################################################################################################################
############################################ Meteo effect/covariates
####################################################################################################################################

############################################ Temperature
par(mar = c(5, 5, 2, 5))
par(mfrow = c(1, 1))
plot(Data0$Date, Data0$Net_demand, type = "l")
par(new = T)
plot(Data0$Date, Data0$Temp, type = "l", col = "red", axes = F, xlab = "", ylab = "")
# plot(Data0$Temp%>%tail(1000), type='l', col='red', axes=F,xlab='',ylab='')
axis(side = 4, col = "red", col.axis = "red")
mtext(side = 4, line = 3, "Temperature", col = "red")
legend("top", c("Net_demand", "Temperature"), col = c("black", "red"), lty = 1, ncol = 1, bty = "n")

col.tr <- adjustcolor(col = "black", alpha = 0.25)
plot(Data0$Temp, Data0$Net_demand, pch = 3, col = col.tr)


plot(Data0$Date %>% head(, n = 7 * 3), Data0$Temp %>% head(, n = 7 * 3), type = "l")
lines(Data0$Date %>% head(, n = 7 * 3), Data0$Temp_s95 %>% head(, n = 7 * 3), col = "blue")
lines(Data0$Date %>% head(, n = 7 * 3), Data0$Temp_s99 %>% head(, n = 7 * 3), col = "red")

plot(Data0$Date %>% head(, n = 7 * 5), Data0$Temp_s99 %>% head(, n = 7 * 5), type = "l")
lines(Data0$Date %>% head(, n = 7 * 5), Data0$Temp_s99_min %>% head(, n = 7 * 5), col = "blue")
lines(Data0$Date %>% head(, n = 7 * 5), Data0$Temp_s99_max %>% head(, n = 7 * 5), col = "red")

par(mfrow = c(1, 1))
col.tr1 <- adjustcolor(col = "black", alpha = 0.25)
col.tr2 <- adjustcolor(col = "red", alpha = 0.25)
plot(Data0$Temp, Data0$Net_demand, pch = 3, col = col.tr1)
points(Data0$Temp_s99, Data0$Net_demand, pch = 3, col = col.tr2)


col.tr <- adjustcolor(col, alpha = 0.25)
par(mfrow = c(3, 1))
plot(Data0$Temp, Data0$Load, pch = 3, col = col.tr[1])
plot(Data0$Temp, Data0$Solar_power, pch = 3, col = col.tr[2])
plot(Data0$Temp, Data0$Wind_power, pch = 3, col = col.tr[3])

############################################ Wind
plot(Data0$Date, Data0$Wind, type = "l")
plot(Data0$Date, Data0$Wind_weighted, type = "l")


par(mfrow = c(3, 1))
plot(Data0$Wind, Data0$Load, pch = 3, col = col[1])
plot(Data0$Wind, Data0$Solar_power, pch = 3, col = col[2])
plot(Data0$Wind, Data0$Wind_power, pch = 3, col = col[3])

par(mfrow = c(1, 1))
plot(Data0$Wind, Data0$Wind_power, pch = 3, col = col[3])
points(Data0$Wind_weighted, Data0$Wind_power, pch = 3, col = col[4])



par(mfrow = c(1, 1))
plot(Data0$Date, Data0$Net_demand, type = "l")
par(new = T)
plot(Data0$Date, Data0$Wind, type = "l", col = "red", axes = F, xlab = "", ylab = "")
# plot(Data0$Temp%>%tail(1000), type='l', col='red', axes=F,xlab='',ylab='')
axis(side = 4, col = "red", col.axis = "red")
mtext(side = 4, line = 3, "Wind", col = "red")
legend("top", c("Net_demand", "Wind"), col = c("black", "red"), lty = 1, ncol = 1, bty = "n")


K <- 7 * 4
smooth_net <- stats::filter(Data0$Net_demand, rep(1 / K, K))
smooth_wind <- stats::filter(Data0$Wind, rep(1 / K, K))
par(mfrow = c(1, 1))
plot(Data0$Date, smooth_net, type = "l")
par(new = T)
plot(Data0$Date, smooth_wind, type = "l", col = "red", axes = F, xlab = "", ylab = "")
# plot(Data0$Temp%>%tail(1000), type='l', col='red', axes=F,xlab='',ylab='')
axis(side = 4, col = "red", col.axis = "red")
mtext(side = 4, line = 3, "Wind", col = "red")
legend("top", c("Net_demand", "Wind"), col = c("black", "red"), lty = 1, ncol = 1, bty = "n")



K <- 7 * 4
smooth_wp <- stats::filter(Data0$Wind_power, rep(1 / K, K))
smooth_wind <- stats::filter(Data0$Wind, rep(1 / K, K))
par(mfrow = c(1, 1))
plot(Data0$Date, smooth_wp, type = "l")
par(new = T)
plot(Data0$Date, smooth_wind, type = "l", col = "red", axes = F, xlab = "", ylab = "")
# plot(Data0$Temp%>%tail(1000), type='l', col='red', axes=F,xlab='',ylab='')
axis(side = 4, col = "red", col.axis = "red")
mtext(side = 4, line = 3, "Wind", col = "red")
legend("top", c("Wind power", "Wind"), col = c("black", "red"), lty = 1, ncol = 1, bty = "n")





############################################ Solar
plot(Data0$Date, Data0$Nebulosity, type = "l")
plot(Data0$Date, Data0$Nebulosity_weighted, type = "l")

K <- 7 * 5
smooth_neb <- stats::filter(Data0$Nebulosity, rep(1 / K, K))
plot(Data0$Date, smooth_neb, type = "l")

par(mfrow = c(3, 1))
plot(Data0$Nebulosity, Data0$Load, pch = 3, col = col[1])
plot(Data0$Nebulosity, Data0$Solar_power, pch = 3, col = col[2])
plot(Data0$Nebulosity, Data0$Wind_power, pch = 3, col = col[3])

sel <- which(year(Data0$Date) >= 2018)
par(mfrow = c(3, 1))
plot(Data0$Nebulosity[sel], Data0$Load[sel], pch = 3, col = col[1])
plot(Data0$Nebulosity[sel], Data0$Solar_power[sel], pch = 3, col = col[2])
plot(Data0$Nebulosity[sel], Data0$Wind_power[sel], pch = 3, col = col[3])

cor(Data0$Nebulosity, Data0$Solar_power)
cor(Data0$Nebulosity[sel], Data0$Solar_power[sel])
cor(Data0$Nebulosity_weighted[sel], Data0$Solar_power[sel])



############################################ Lag
names(Data0)

plot(Data0$Net_demand.7, Data0$Net_demand, pch = 3)
plot(Data0$Net_demand.1, Data0$Net_demand, pch = 3)

cor(Data0$Net_demand.1, Data0$Net_demand)
cor(Data0$Net_demand.7, Data0$Net_demand)


############################################ Holidays
boxplot(Net_demand ~ as.factor(Christmas_break), data = Data0[which(Data0$DLS == 0), ])
boxplot(Net_demand ~ Summer_break, data = Data0[which(Data0$DLS == 1), ])
boxplot(Net_demand ~ BH, data = Data0)


############################################ DLS
boxplot(Load ~ DLS, data = Data0)

######################################### train/Test

par(mfrow = c(1, 2))
hist(Data0$Temp)
hist(Data1$Temp)

range(Data0$Temp)
range(Data1$Temp)

par(mfrow = c(1, 1))
hist(Data0$Temp, xlim = range(Data0$Temp, Data1$Temp), col = "lightblue", breaks = 50, main = "Temp")
par(new = T)
hist(Data1$Temp, xlim = range(Data0$Temp, Data1$Temp), col = adjustcolor("red", alpha.f = 0.5), , breaks = 50, main = "")

par(mfrow = c(1, 1))
hist(Data0$Nebulosity, xlim = range(Data0$Nebulosity, Data1$Nebulosity), col = "lightblue", breaks = 50, main = "Neb")
par(new = T)
hist(Data1$Nebulosity, xlim = range(Data0$Nebulosity, Data1$Nebulosity), col = adjustcolor("red", alpha.f = 0.5), , breaks = 50, main = "")


sel <- which(year(Data0$Date) >= 2018)
par(mfrow = c(1, 1))
hist(Data0$Wind[sel], xlim = range(Data0$Wind, Data1$Wind), col = "lightblue", breaks = 50, main = "Wind")
par(new = T)
hist(Data1$Wind, xlim = range(Data0$Wind, Data1$Wind), col = adjustcolor("red", alpha.f = 0.5), , breaks = 50, main = "")



sel <- which(year(Data0$Date) >= 2018)
par(mfrow = c(1, 1))
hist(Data0$Nebulosity[sel], xlim = range(Data0$Nebulosity, Data1$Nebulosity), col = "lightblue", breaks = 50)
par(new = T)
hist(Data1$Nebulosity, xlim = range(Data0$Nebulosity, Data1$Nebulosity), col = adjustcolor("red", alpha.f = 0.5), , breaks = 50)
