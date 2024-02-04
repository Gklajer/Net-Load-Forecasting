rm(list = objects())
graphics.off()
library(tidyverse)
library(lubridate)
library(forecast)


Data0 <- read_delim("Data/train.csv", delim = ",")
Data1 <- read_delim("Data/test.csv", delim = ",")


Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

Data0$WeekDays <- as.factor(Data0$WeekDays)
Data1$WeekDays <- as.factor(Data1$WeekDays)

summary(Data0)


sel_a <- which(Data0$Year <= 2021)
sel_b <- which(Data0$Year > 2021)

################################### analyse de la demande nette

############################################ trend
plot(Data0$Date, Data0$Net_demand, type = "l", xlim = range(Data0$Date, Data1$Date))

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
