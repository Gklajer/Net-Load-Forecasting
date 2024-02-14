fitmod <- function(eq, block) {
  mod <- lm(eq, data = Data0[-block, ])
  mod.cvpred <- predict(mod, newdata = Data0[block, ])
  return(mod.cvpred)
}