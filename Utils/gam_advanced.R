rm(list=objects())
###############packages
library(mgcv)
library(yarrr)
library(magrittr)
library(forecast)
library(tidyverse)
source('R/score.R')



Data0 <- read_delim("Data/train.csv", delim=",")
Data1<- read_delim("Data/test.csv", delim=",")

range(Data1$Date)



range(Data0$Date)

Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)


sel_a <- which(Data0$Year<=2021)
sel_b <- which(Data0$Year>2021)



Nblock<-10
borne_block<-seq(1, nrow(Data0), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))



blockRMSE<-function(equation, block)
{
  g<- gam(as.formula(equation), data=Data0[-block,])
  forecast<-predict(g, newdata=Data0[block,])
  return(forecast)
}




#####model 6
equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + as.factor(WeekDays) +BH
Block_residuals<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmse6 <- rmse(Data0$Load, Block_residuals)
rmse6
gam6<-gam(equation, data=Data0[sel_a,])
sqrt(gam6$gcv.ubre)
summary(gam6)

gam6.forecast<-predict(gam6,  newdata= Data0[sel_b,])
gam6$gcv.ubre%>%sqrt
rmse(Data0[sel_b,]$Load, gam6.forecast)


#####model 7 variante ti

equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + as.factor(WeekDays) +BH  + te(Temp_s99, Temp, bs=c('cr','cr'), k=c(10,10))

Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse_te <- rmse(Data0$Load, Block_forecast)
rmse_te
gam7<-gam(equation, data=Data0[sel_a,])
summary(gam7)
gam7.forecast<-predict(gam7,  newdata= Data0[sel_b,])
gam7$gcv.ubre%>%sqrt
rmse(Data0[sel_b,]$Load, gam7.forecast)


equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + ti(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  ti(Temp_s99,k=10, bs='cr') + as.factor(WeekDays) +BH  + ti(Temp_s99, Temp, bs=c('cr','cr'), k=c(10,10))

Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmse_ti <- rmse(Data0$Load, Block_forecast)
rmse_ti
gam7_ti<-gam(equation, data=Data0[sel_a,])
summary(gam7_ti)
gam7_ti.forecast<-predict(gam7_ti,  newdata= Data0[sel_b,])
gam7_ti$gcv.ubre%>%sqrt
rmse(Data0[sel_b,]$Load, gam7_ti.forecast)






##############################################################################
#############Anova selection
##############################################################################


equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)

equation_by <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr', by= as.factor(WeekDays))+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)


equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + as.factor(WeekDays) +BH

equation_by <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr',  by= as.factor(WeekDays))+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + as.factor(WeekDays) +BH



gam6<-gam(equation, data=Data0)
gam6_by<-gam(equation_by, data=Data0)
gam6$gcv.ubre%>%sqrt
gam6_by$gcv.ubre%>%sqrt

summary(gam6_by)
anova(gam6, gam6_by, test = "Chisq") ####p value <0.05 interaction is significant

gam6_by<-gam(equation_by, data=Data0[sel_a,])
gam6_by.forecast<-predict(gam6_by,  newdata= Data0[sel_b,])
rmse(Data0[sel_b,]$Load, gam6_by.forecast)

rmse(Data0[sel_b,]$Load, gam6.forecast)

###exemple2
equation <- Load~ ti(Load.1, bs='cr')+ ti(Load.7, bs='cr')
equation2 <- Load~ ti(Load.1, bs='cr')+ ti(Load.7, bs='cr') + ti(Load.1, Load.7)
fit1<-gam(equation, data=Data0)
fit2<-gam(equation2, data=Data0)

anova(fit1, fit2, test = "Chisq") ####p value <0.05 interaction is significant



########################################################################################################
########shrinkage approach
########################################################################################################

Data0$Var1 <- rnorm(nrow(Data0), mean=0, sd=sd(Data0$Load))


equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr', by= as.factor(WeekDays))+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + as.factor(WeekDays) +BH  + BH_before + BH_after+ s(Temp_s95) +ti(Nebulosity_weighted) + ti(Wind_weighted) +ti(Wind_weighted, Temp) +s(Var1)

gam8<-gam(equation, data=Data0[sel_a,])
summary(gam8)
gam8$gcv.ubre%>%sqrt
gam8.forecast<-predict(gam8,  newdata= Data0[sel_b,])
rmse(Data0[sel_b,]$Load, gam8.forecast)


equation_cs <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr', by= as.factor(WeekDays))+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + as.factor(WeekDays) +BH  + BH_before + BH_after+ s(Temp_s95) +ti(Nebulosity_weighted) + ti(Wind_weighted) +ti(Wind_weighted, Temp) + s(Var1, bs='cs')
# equation_cs <- Load~s(as.numeric(Date),k=3, bs='cs') + s(toy,k=30, bs='cr') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cs',  by= as.factor(WeekDays))+ s(Load.7, bs='cr') +
#   s(Temp_s99,k=10, bs='cs') + as.factor(WeekDays) +BH  + BH_before + BH_after +s(Temp_s95, bs='cs') +ti(Nebulosity_weighted, bs='ts') + ti(Wind_weighted, bs='ts')+ti(Wind_weighted, Temp, bs='ts') + s(Var1, bs='cs')
gam8_cs<-gam(equation_cs, data=Data0[sel_a,])
summary(gam8_cs)
gam8_cs.forecast<-predict(gam8_cs,  newdata= Data0[sel_b,])
gam8_cs$gcv.ubre%>%sqrt
rmse(Data0[sel_b,]$Load, gam8_cs.forecast)




##toy test à faire en retirant cc de l'effet toy
terms <- predict(gam8, newdata=Data0, type='terms')
terms_cs <- predict(gam8_cs, newdata=Data0, type='terms')
sel.column <- which(colnames(terms)=="s(toy)")
o <- order(Data0$toy)
plot(Data0$toy[o] , terms[o, sel.column], type='l', ylim=range(terms[o, sel.column], terms_cs[o, sel.column]))
lines(Data0$toy[o] , terms_cs[o, sel.column], col='red')


##Wind_weighted
terms <- predict(gam8, newdata=Data0, type='terms')
terms_cs <- predict(gam8_cs, newdata=Data0, type='terms')
sel.column <- which(colnames(terms)=="ti(Wind_weighted)")
o <- order(Data0$Wind_weighted)
plot(Data0$Wind_weighted[o] , terms[o, sel.column], type='l', ylim=range(terms[o, sel.column], terms_cs[o, sel.column]))
lines(Data0$Wind_weighted[o] , terms_cs[o, sel.column], col='red')


##Var1
terms <- predict(gam8, newdata=Data0, type='terms')
terms_cs <- predict(gam8_cs, newdata=Data0, type='terms')
sel.column <- which(colnames(terms)=="s(Var1)")
o <- order(Data0$Wind_weighted)
plot(Data0$Var1[o] , terms[o, sel.column], type='l', ylim=range(terms[o, sel.column], terms_cs[o, sel.column]))
lines(Data0$Var1[o] , terms_cs[o, sel.column], col='red')



########################################################################################################
########double penalty shrinkage
########################################################################################################

equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr', by= as.factor(WeekDays))+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + as.factor(WeekDays) +BH  + BH_before + BH_after+ s(Temp_s95) +ti(Nebulosity_weighted) + ti(Wind_weighted) +ti(Wind_weighted, Temp) + s(Var1, bs='cs')

gam9_select<-gam(equation, data=Data0[sel_a,], select=TRUE, gamma=1.5)
summary(gam9_select)
gam9_select$gcv.ubre%>%sqrt
gam9_select.forecast<-predict(gam9_select,  newdata= Data0[sel_b,])
rmse(Data0[sel_b,]$Load, gam9_select.forecast)



##Var1
terms <- predict(gam8, newdata=Data0, type='terms')
terms_cs <- predict(gam8_cs, newdata=Data0, type='terms')
terms_select <- predict(gam9_select, newdata=Data0, type='terms')
sel.column <- which(colnames(terms)=="s(Var1)")
o <- order(Data0$Wind_weighted)
plot(Data0$Var1[o] , terms[o, sel.column], type='l', ylim=range(terms[o, sel.column], terms_cs[o, sel.column]))
lines(Data0$Var1[o] , terms_cs[o, sel.column], col='red')
lines(Data0$Var1[o] , terms_select[o, sel.column], col='blue')






##################################################################
######online learning
##################################################################

equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr', by= as.factor(WeekDays))+ s(Load.7, bs='cr')  + as.factor(WeekDays) +BH 

gam9<-gam(equation%>%as.formula, data=Data0[sel_a,])
gam9.forecast <- predict(gam9, newdata=Data0[sel_b,])

X <- predict(gam9, newdata=Data0, type='terms')
###scaling columns
for (j in 1:ncol(X))
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
X <- cbind(X,1)
d <- ncol(X)

y <- Data0$Load

# static 
ssm <- viking::statespace(X, y)
ssm
gam9.kalman.static <- ssm$pred_mean%>%tail(length(sel_b))

rmse(y=Data0$Load[sel_b], ychap=gam9.forecast)
rmse(y=Data0$Load[sel_b], ychap=gam9.kalman.static)


# dynamic
# using iterative grid search
ssm_dyn <- viking::select_Kalman_variances(ssm, X[sel_a, ], y[sel_a], q_list = 2^(-30:0), p1 = 1, 
                                           ncores = 6)
#saveRDS(ssm_dyn, "Results/ssm_dyn.RDS")
ssm_dyn <- readRDS("Results/ssm_dyn.RDS")
ssm_dyn <- predict(ssm_dyn, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(length(sel_b))
rmse(y=Data0$Load[sel_b], ychap=gam9.kalman.Dyn)



plot(ssm_dyn, pause=F, window_size = 14, date = Data0$Date, sel = sel_b)


# using expectation-maximization
ssm_em <- viking::select_Kalman_variances(ssm, X[sel_a,], y[sel_a], method = 'em', n_iter = 10^3,
                                          Q_init = diag(d), verbose = 10, mode_diag = T)
ssm_em <- predict(ssm_em, X, y, type='model', compute_smooth = TRUE)

#saveRDS(ssm_em, "Results/ssm_em.RDS")
ssm_em <-readRDS("Results/ssm_em.RDS")

gam9.kalman.Dyn.em <- ssm_em$pred_mean%>%tail(length(sel_b))


plot(ssm_em, pause=F, window_size = 14, date = Data0$Date, sel = sel_b)
rmse(y=Data0$Load[sel_b], ychap=gam9.kalman.Dyn.em)

plot(Data0$Date[sel_b], Data0$Load[sel_b], type='l')
lines(Data0$Date[sel_b], gam9.forecast, col='red')
lines(Data0$Date[sel_b], gam9.kalman.static, col='blue')
lines(Data0$Date[sel_b], gam9.kalman.Dyn, col='green')
lines(Data0$Date[sel_b], gam9.kalman.Dyn.em, col='purple')



r <- range(cumsum(Data0$Load[sel_b]-gam9.kalman.Dyn), cumsum(Data0$Load[sel_b]- gam9.forecast))
plot(Data0$Date[sel_b], cumsum(Data0$Load[sel_b]- gam9.forecast), type='l', ylim=r)
lines(Data0$Date[sel_b], cumsum(Data0$Load[sel_b]-gam9.kalman.static), col='blue')
lines(Data0$Date[sel_b], cumsum(Data0$Load[sel_b]-gam9.kalman.Dyn), col='green')
lines(Data0$Date[sel_b], cumsum(Data0$Load[sel_b]-gam9.kalman.Dyn.em), col='purple')




ssm_dyn2 <- readRDS("Results/ssm_dyn.RDS")
ssm_dyn2$kalman_params$Q <- ssm_dyn2$kalman_params$Q*10
ssm_dyn2 <- predict(ssm_dyn2, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn2 <- ssm_dyn2$pred_mean%>%tail(length(sel_b))
rmse(y=Data0$Load[sel_b], ychap=gam9.kalman.Dyn2)

r <- range(cumsum(Data0$Load[sel_b]-gam9.kalman.Dyn), cumsum(Data0$Load[sel_b]- gam9.forecast))
plot(Data0$Date[sel_b], cumsum(Data0$Load[sel_b]- gam9.forecast), type='l', ylim=r)
lines(Data0$Date[sel_b], cumsum(Data0$Load[sel_b]-gam9.kalman.static), col='blue')
lines(Data0$Date[sel_b], cumsum(Data0$Load[sel_b]-gam9.kalman.Dyn), col='green')
lines(Data0$Date[sel_b], cumsum(Data0$Load[sel_b]-gam9.kalman.Dyn.em), col='purple')
lines(Data0$Date[sel_b], cumsum(Data0$Load[sel_b]-gam9.kalman.Dyn2), col='orange')



############################################################################################################
##################qgam
############################################################################################################
library(qgam)
equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + 
  s(Load.1, bs='cr', by= as.factor(WeekDays))+ s(Load.7, bs='cr')  + as.factor(WeekDays) +BH 
equation_var <- ~  s(Temp,k=10, bs='cr') + s(Load.1)
gqgam <- qgam(list(equation, equation_var), data=Data0[sel_a,], discrete=TRUE, qu=0.95)

gqgam.forecast <- predict(gqgam, newdata=Data0[sel_b,])
pinball_loss(y=Data0$Load[sel_b], gqgam.forecast, quant=0.95, output.vect=FALSE)

plot(Data0$Load[sel_b], type='l')
lines(gqgam.forecast, col='red')


gqgam95 <- qgam(list(equation, equation_var), data=Data0[sel_a,], discrete=TRUE, qu=0.95)
#gqgam05 <- qgam(list(equation, equation_var), data=Data0[sel_a,], discrete=TRUE, qu=0.05)
#X <- cbind(predict(gqgam05, newdata=Data0, type='terms'), predict(gqgam95, newdata=Data0, type='terms'))

X <-  predict(gqgam95, newdata=Data0, type='terms')
###scaling columns
for (j in 1:ncol(X))
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
X <- cbind(X,1)
d <- ncol(X)

# static 
ssm_q <- viking::statespace(X, y)
ssm_q
gqgam.kalman.static <- ssm_q$pred_mean%>%tail(length(sel_b))
rmse(y=Data0$Load[sel_b], ychap=gqgam.kalman.static)


# dynamic
# using iterative grid search
ssm_dyn_q <- viking::select_Kalman_variances(ssm_q, X[sel_a, ], y[sel_a], q_list = 2^(-30:0), p1 = 1, 
                                           ncores = 6)
saveRDS(ssm_dyn_q, "Results/ssm_dyn_q.RDS")

ssm_dyn_q <- readRDS("Results/ssm_dyn_q.RDS")
ssm_dyn_q <- predict(ssm_dyn_q, X, y, type='model', compute_smooth = TRUE)
gqgam.kalman.Dyn <- ssm_dyn_q$pred_mean%>%tail(length(sel_b))
rmse(y=Data0$Load[sel_b], ychap=gqgam.kalman.Dyn)












gqgam05 <- qgam(list(equation, equation_var), data=Data0[sel_a,], discrete=TRUE, qu=0.5)
gqgam05.forecast <- predict(gqgam, newdata=Data0[sel_b,])
rmse(y=Data0$Load[sel_b], ychap=gqgam05.forecast)
