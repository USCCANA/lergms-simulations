simfun <- function(d, boot = FALSE) {
  
  # Simulating networks
  nets <- d$nets
  
  # Checking balance: If the sampling heavily unbalanced, then return NAs
  prop <- lapply(nets, `diag<-`, NA)
  prop <- unlist(prop, recursive = TRUE)
  prop <- sum(prop == 1, na.rm = TRUE)/sum(!is.na(prop))
  
  if (min(prop, 1-prop) < .1) {
    null <- d$par
    null[] <- rep(NA, length(null))
    return(
      list(
        coef    = null,
        vcov    = matrix(NA, nrow = length(null), ncol = length(null),
                      dimnames = list(names(null), c("2.5 %", "97.5 %"))),
        balance = prop
      )
    )
  }
    
  # Else we estimate the lergm
  estimates <- if (!boot) 
    ergmito(nets ~ edges + mutual, optim.args=list(upper=3, lower=-3))
  else
    ergmito_boot(nets ~ edges + mutual, ncpus = 1L, R=1000, optim.args=list(upper=3, lower=-3))
  
  list(
    coef    = coef(estimates),
    vcov    = confint(estimates),
    balance = prop
  )
    
    
}
