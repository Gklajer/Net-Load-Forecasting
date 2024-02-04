rm(list=objects())
library(tidyverse)
library(lubridate)
library(forecast)

setwd("\\Master\\S2\\Modelisation predictive")
#Data0 <- read_delim("Data/train.csv", delim=",")
#Data1<- read_delim("Data/test.csv", delim=",")


Data0 <- read_csv('train.csv')
Data1 <- read_csv('test.csv')


Data0$Temp_res <- (33 + (Data0$Temp - 273.15 - 33)*(0.474 + 0.454*sqrt(Data0$Wind)) - 0.0454*Data0$Wind)+273.15 #Formule de Siple pour la temperature ressenti
plot(Data0$Temp, Data0$Temp_res)