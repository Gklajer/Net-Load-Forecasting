rm(list=objects())
###############packages
library(mgcv)
library(yarrr)
library(magrittr)
library(forecast)
library(tidyverse)
library(ranger)
source('Utils/score.R')

Data0 <- read_delim("Data/train.csv", delim=",")
Data1<- read_delim("Data/test.csv", delim=",")

range(Data0$Date)

Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)


sel_a <- which(Data0$Year<=2021)
sel_b <- which(Data0$Year>2021)


equation <- Load~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS 

rf <- ranger(equation, data=Data0[sel_a,], importance =  'permutation')
rf$r.squared

rf.forecast<-predict(rf,  data= Data0[sel_b,])$prediction
rmse1.forecast <- rmse.old(Data0$Load[sel_b]-rf.forecast)

  
#############importance plot
imp <- rf$variable.importance
sort(imp)
o <- order(imp, decreasing=T)
nom <- names(imp)
plot(c(1:length(imp)), imp[o], type='h', ylim = c(0, max(imp) + max(imp)/5), xlab='', ylab='Importance (permutation)')
K <- length(imp)
text(tail(c(1:length(imp)), K), tail(imp[o]+max(imp/8), K), labels= tail(nom[o], K), pos=3, srt=90, adj=1)
points(c(1:length(imp)), imp[o], pch=20)



#####influence des paramètres
####nTree
rmse.fitted <- list()
rmse.forecast <- list()
rmse.oob <- list()

#ntree
 
rfTest.fitted<-lapply(rfTest, predict, data=Data0[sel_a,])
rfTest.forecast<-lapply(rfTest, predict, data=Data0[sel_b,])

rmse.fitted$ntree<-lapply(rfTest.fitted, function(x){sqrt(mean((Data0[sel_a,]$Load-x$prediction)^2))})%>%unlist()
rmse.forecast$ntree<-lapply(rfTest.forecast, function(x){sqrt(mean((Data0[sel_b,]$Load-x$prediction)^2))})%>%unlist()
rmse.oob$ntree <-  lapply(rfTest, function(x){sqrt(x$prediction.error) %>% tail(, n=1) })%>%unlist()

par(mfrow=c(1,1))
plot(ntest, rmse.fitted$ntree,type='l', col='royalblue2', ylim=range(rmse.fitted,rmse.forecast))
lines(ntest, rmse.forecast$ntree, col='orangered2')
lines(ntest, rmse.oob$ntree, col='dark green')


#mtry
mtry<-seq(1,10,by=1)
rfTest<-lapply(mtry, function(m){ranger(equation, mtry = m, data=Data0[sel_a,], num.trees=100)})
rfTest.fitted<-lapply(rfTest, predict, data=Data0[sel_a,])
rfTest.forecast<-lapply(rfTest, predict, data=Data0[sel_b,])
rmse.fitted$mtry<-lapply(rfTest.fitted, function(x){sqrt(mean((Data0[sel_a,]$Load-x$prediction)^2))})%>%unlist()
rmse.forecast$mtry<-lapply(rfTest.forecast, function(x){sqrt(mean((Data0[sel_b,]$Load-x$prediction)^2))})%>%unlist()
rmse.oob$mtry <-  lapply(rfTest, function(x){sqrt(x$prediction.error) %>% tail(, n=1) })%>%unlist()

par(mfrow=c(1,1))
plot(mtry, rmse.fitted$mtry,type='l', col='royalblue2', ylim=range(rmse.fitted,rmse.forecast))
lines(mtry, rmse.forecast$mtry, col='orangered2')
lines(mtry, rmse.oob$mtry, col='dark green')
abline(v=sqrt(16))


###################################################################################################################
#############################################quantile RF
###################################################################################################################
qrf<- ranger::ranger(equation, data = Data0[sel_a,], importance =  'permutation', seed=1, , quantreg=TRUE)
quant=0.95
qrf.forecast <- predict(qrf, data=Data0[sel_b,], quantiles =quant, type = "quantiles")$predictions
pinball_loss(Data0[sel_b,]$Load, qrf.forecast,quant) 



###################################################################################################################
#############################################GAM/RF
###################################################################################################################

####estimation of block CV residuals


blockRMSE<-function(equation, block, data)
{
  g<- gam(as.formula(equation), data=data[-block,])
  forecast<-predict(g, newdata=data[block,])
  return(data[block,]$Load-forecast)
} 


Nblock<-10
borne_block<-seq(1, nrow(Data0[sel_a,]), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))



equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + as.factor(WeekDays) +BH
Block_residuals<-lapply(block_list, blockRMSE, equation=equation, data=Data0[sel_a,])%>%unlist
length(Block_residuals)

####estimation of GAM, GAM effects
g <- gam(equation, data = Data0[sel_a,])
g.forecast <- predict(g, newdata=Data0)
terms0 <- predict(g, newdata=Data0, type='terms')
colnames(terms0) <- paste0("gterms_", c(1:ncol(terms0)))

residuals <- c(Block_residuals, Data0[sel_b,]$Load-g.forecast[sel_b])
Data0_rf <- data.frame(Data0, terms0)
Data0_rf$res <- residuals
Data0_rf$res.48 <- c(residuals[1], residuals[1:(length(residuals)-1)])
Data0_rf$res.336 <- c(residuals[1:7], residuals[1:(length(residuals)-7)])


cov <- "Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS  + "
gterm <-paste0("gterms_", c(1:ncol(terms0)))
gterm <- paste0(gterm, collapse='+')
cov <- paste0(cov, gterm, collapse = '+')
formule_rf <- paste0("res", "~", cov)

rf_gam<- ranger::ranger(formule_rf, data = Data0_rf[sel_a,], importance =  'permutation')
rf_gam$r.squared

rf_gam.forecast <- predict(rf_gam, data = Data0_rf)$predictions+ g.forecast
rmse(Data0$Load[sel_a],rf_gam.forecast[sel_a])
rmse(Data0$Load[sel_b],rf_gam.forecast[sel_b])


res <- Data0$Load[sel_a] - rf_gam.forecast[sel_a]
hist(res, breaks=100)
mean(res)
quant <- qnorm(0.95, mean= mean(res), sd= sd(res))
pinball_loss(y=Data0$Load[sel_b], rf_gam.forecast[sel_b]+quant, quant=0.95, output.vect=FALSE)

grid.q <- seq(0.9, 0.9999999, length=100)
pbscore <- grid.q
for(i in c(1:length(grid.q)))
{
  quant <- qnorm(grid.q[i], mean= mean(res), sd= sd(res))
  pbscore[i] <- pinball_loss(y=Data0$Load[sel_b], rf_gam.forecast[sel_b]+quant, quant=0.95, output.vect=FALSE)
  
}
plot(grid.q, pbscore)



m <- mean(Data0$Load[sel_b]-rf_gam.forecast[sel_b])
quant <- qnorm(0.9, mean= m, sd= sd(res))
pinball_loss(y=Data0$Load[sel_b], rf_gam.forecast[sel_b]+quant, quant=0.95, output.vect=FALSE)


grid.q <- seq(0.9, 0.9999999, length=100)
pbscore <- grid.q
for(i in c(1:length(grid.q)))
{
  m <- mean(Data0$Load[sel_b]-rf_gam.forecast[sel_b])
  quant <- qnorm(grid.q[i], mean= m, sd= sd(res))
  pbscore[i] <- pinball_loss(y=Data0$Load[sel_b], rf_gam.forecast[sel_b]+quant, quant=0.95, output.vect=FALSE)
  
}
plot(grid.q, pbscore)


par(mfrow=c(2,1))
res_a <- Data0$Load[sel_a] - rf_gam.forecast[sel_a]
res_b <- Data0$Load[sel_b] - rf_gam.forecast[sel_b]

hist(res_a, breaks=100, xlim=range(res_a, res_b))
hist(res_b, breaks=100, xlim=range(res_a, res_b))


grid.q <- seq(0.9, 0.9999999, length=100)
pbscore <- grid.q
for(i in c(1:length(grid.q)))
{
  m <- mean(res_b)
  s <- sd(res_b)
  quant <- qnorm(grid.q[i], mean= m, sd= s)
  pbscore[i] <- pinball_loss(y=Data0$Load[sel_b], rf_gam.forecast[sel_b]+quant, quant=0.95, output.vect=FALSE)
  
}
par(mfrow=c(1,1))
plot(grid.q, pbscore, type='l')




qrf_gam<- ranger::ranger(formule_rf, data = Data0_rf[sel_a,], importance =  'permutation', seed=1, , quantreg=TRUE)
quant=0.95
qrf_gam.forecast <- predict(qrf_gam, data=Data0_rf[sel_b,], quantiles =quant, type = "quantiles")$predictions%>%as.numeric+g.forecast[sel_b]
pinball_loss(Data0[sel_b,]$Load, qrf_gam.forecast,quant) 



#############importance plot
imp <- rf_gam$variable.importance
sort(imp)
o <- order(imp, decreasing=T)
nom <- names(imp)
plot(c(1:length(imp)), imp[o], type='h', ylim = c(0, max(imp) + max(imp)/5), xlab='', ylab='Importance (permutation)')
K <- length(imp)
text(tail(c(1:length(imp)), K), tail(imp[o]+max(imp/8), K), labels= tail(nom[o], K), pos=3, srt=90, adj=1)
points(c(1:length(imp)), imp[o], pch=20)







########################################################boosting
library(gbm)
equation <- Load~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS 

gbm0 <- gbm(equation,distribution = "gaussian" , data = Data0[sel_a,],n.trees = 500, interaction.depth = 10,
         n.minobsinnode = 5, shrinkage = 0.01, bag.fraction = 0.5, train.fraction = 1,
         keep.data = FALSE, n.cores = 4)

best.iter=gbm.perf(gbm0,method="OOB", plot.it = TRUE,oobag.curve = TRUE)    
best.iter    ###200 aurai suffit
NTreeOpt <- which.min(-cumsum(gbm0$oobag.improve))
plot(-cumsum(gbm0$oobag.improve),type='l')

gbm0.forecast <- predict(gbm0,n.trees=NTreeOpt,single.tree=FALSE,newdata=Data0[sel_b,])
rmse(y=Data0[sel_b,]$Load, ychap=gbm0.forecast)


Ntree <- 2000
gbm1 <- gbm(equation,distribution = "gaussian" , data = Data0[sel_a,],n.trees = Ntree, interaction.depth = 10,
            n.minobsinnode = 5, shrinkage = 0.01, bag.fraction = 0.5, train.fraction = 1,
            keep.data = FALSE, n.cores = 8)

gbm1 <- gbm(equation,distribution = "gaussian" , data = Data0[sel_a,],n.trees = Ntree, interaction.depth = 10,
            n.minobsinnode = 5, shrinkage = 0.02, bag.fraction = 0.5, train.fraction = 1,
            keep.data = FALSE, n.cores = 8)

best.iter=gbm.perf(gbm1,method="OOB", plot.it = TRUE,oobag.curve = TRUE)    
best.iter    
NTreeOpt <- which.min(-cumsum(gbm1$oobag.improve))
plot(-cumsum(gbm1$oobag.improve),type='l')
gbm1.forecasts<-lapply(c(1:Ntree),function(x){predict(gbm1,n.trees=x,single.tree=FALSE, newdata=Data0[sel_b,])})
gbm1.forecast.error<-unlist(lapply(gbm1.forecasts,function(x){sqrt(mean((Data0[sel_b,]$Load-x)^2))}))

plot(sqrt(gbm1$train.error),type='l')
lines(gbm1.forecast.error,col='purple')

gbm1.forecast <- predict(gbm1,n.trees=NTreeOpt,single.tree=FALSE,newdata=Data0[sel_b,])
rmse(y=Data0[sel_b,]$Load, ychap=gbm1.forecast)





