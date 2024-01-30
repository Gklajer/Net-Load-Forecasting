rm(list = objects())
library(tidyverse)
train <- read_csv("Data/train.csv")
test <- read_csv("Data/test.csv")
names(train)
names(test)

range(train$Date)
range(test$Date)




###############################################################################################################################################################################
Data0 <- train
Data0$Time <- as.numeric(Data0$Date)
Data1 <- test
Data1$Time <- as.numeric(Data1$Date)

equation <- Net_demand ~  Time + toy + Temp + Net_demand.1 + Net_demand.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break + Christmas_break +
  Temp_s95_min + Temp_s99_min + DLS

set.seed(100)
rf <- ranger::ranger(equation, data = Data0, importance = "permutation", quantreg = TRUE)
rf$r.squared


rf.forecast <- predict(rf, data = Data1, quantiles = 0.95, type = "quantiles")$predictions

submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
submit$Net_demand <- rf.forecast
write.table(submit, file = "Data/submission_rf_095.csv", quote = F, sep = ",", dec = ".", row.names = F)
