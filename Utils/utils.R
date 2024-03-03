fitmod <- function(eq, block) {
  mod <- lm(eq, data = Data0[-block, ])
  mod.cvpred <- predict(mod, newdata = Data0[block, ])
  return(mod.cvpred)
}

train_eval <- function(mod_cls, eq, train_data, test_data, is.rq=F, ...) {
  mod <- mod_cls(as.formula(eq), data=train_data, ...)
  pred = predict(mod, newdata = test_data)

  if(is.rq) return(list(pred=pred))
  
  res = test_data[,as.character(eq[[2]])] - pred
  return(list(pred=pred, res=res))
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol

time_cv <- function(mod_cls, eq, data, trainset_size, testset_size=1, nb_eval=0, step=0, type="rolling", is.rq=F, quant_fun=quantile, quant_p=.95, pb_p=.95, ...) {
  trainval_size = trainset_size + testset_size
  excess_data_size = nrow(data) - trainval_size
  
  try(if(!(type %in% c("rolling", "window"))) stop("Argument type must either be rolling or window"))
  
  try(if(!(all(is.wholenumber(c(trainset_size, testset_size, nb_eval, step))))) 
    stop("The arguments trainset_size, testset_size, nb_eval and step must be whole numbers!"))
  
  if(nb_eval == 0 & step == 0) {
    step = testset_size
    nb_eval = floor(excess_data_size / step) + 1
  }
  
  try(if(nb_eval < 1 | excess_data_size < (nb_eval - 1)) 
    stop("Wrong combination of trainset_size, testset_size and nb_eval!"))
  
  maxstep = ifelse(nb_eval == 1, Inf, floor(excess_data_size / (nb_eval - 1)))

  step = ifelse(step == 0, maxstep, step)
  try(if(step < 1 | step > maxstep) 
    stop("Argument step provided is not valid!"))
  
  cv = list(idx=list(), pred=as.data.frame(matrix(nrow = nb_eval, ncol = testset_size)))
  Tnames = paste("T", 1:testset_size, sep = "")
  colnames(cv$pred) <- Tnames
  
  if(!is.rq) cv$res = list(val = cv$pred)
  
  for(i in 1:nb_eval) {
    cv$idx$start[i] = switch(type, "rolling"=1, "window"=1 + (i-1)*step)
    cv$idx$mid[i] = (1 + (i-1)*step) + trainset_size - 1 
    cv$idx$end[i] = (cv$idx$mid[i] + 1) + testset_size - 1
    
    test_data = data[(cv$idx$mid[i]+1):cv$idx$end[i],]
    
    result = train_eval(mod_cls, eq, 
                    train_data = data[cv$idx$start[i]:cv$idx$mid[i],], 
                    test_data = test_data, is.rq = is.rq, ...)

    cv$pred[i,] <- result$pred
    
    if(is.rq) {
      cv$res$pb[i] <- pinball_loss(test_data[,as.character(eq[[2]])], 
                                   result$pred, pb_p,
                                   output.vect = T)
      next
    }
  
    cv$res$val[i,] <- result$res
    
    cv$res$mean[i] <- mean(result$res)
    
    cv$res$rmse[i] <- rmse.old(result$res)
    
    cv$res$quant[i] <- ifelse(identical(quant_fun, quantile), 
                              quantile(result$res, quant_p),
                              quant_fun(quant_p, mean(result$res), sd(result$res)))
    
    cv$res$pb[i] <-  pinball_loss(test_data[,as.character(eq[[2]])], 
                                  result$pred + cv$res$quant[i], pb_p, 
                                  output.vect = T)
  }
  
  return(cv)
}

which.median <- function(x) which.min(x[x>=median(x)])

sliding_quantile <- function(x, window_size, quantile_value) {
  slide_dbl(x, .f = ~ quantile(.x, quantile_value), .before = window_size-1, .complete = TRUE)
}

create_submission <- function(mod_cls, eq, train_data = Data0, pred_data = Data1, quant=0, ...) {
  mod = mod_cls(as.formula(eq), data=train_data, ...)
  
  mod.forecast = predict(mod, newdata = pred_data)
  
  submit <- read_delim(file = "Data/sample_submission.csv", delim = ",")
  submit$Net_demand <- mod.forecast + quant
  write.table(submit, file = "Data/submission_mod.csv", quote = F, sep = ",", dec = ".", row.names = F)
}