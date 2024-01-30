rmse <- function(y, ychap, digits = 0) {
  return(round(sqrt(mean((y - ychap)^2, na.rm = TRUE)), digits = digits))
}

mape <- function(y, ychap) {
  return(round(100 * mean(abs(y - ychap) / abs(y)), digits = 2))
}


rmse.old <- function(residuals, digits = 0) {
  return(round(sqrt(mean((residuals)^2, na.rm = TRUE)), digits = digits))
}


absolute_loss <- function(y, yhat) {
  mean(abs(y - yhat), na.rm = TRUE)
}

bias <- function(y, yhat) {
  mean(y - yhat, na.rm = TRUE)
}




pinball_loss <- function(y, yhat_quant, quant, output.vect = FALSE) {
  yhat_quant <- as.matrix(yhat_quant)
  pinball_loss <- 0
  nq <- ncol(yhat_quant)
  loss_q <- array(0, dim = nq)

  for (q in 1:nq) {
    loss_q[q] <- mean(((y - yhat_quant[, q]) * (quant[q] - (y < yhat_quant[, q]))), na.rm = T)
    # pinball_loss <- pinball_loss + loss_q /nq
    # print(pinball_loss)
  }
  if (output.vect == FALSE) {
    pinball_loss <- mean(loss_q)
  }
  if (output.vect == TRUE) {
    pinball_loss <- loss_q
  }
  return(pinball_loss)
}
