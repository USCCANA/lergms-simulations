library(ergmito)

simfun <- function(d, boot = FALSE) {
  
  # Simulating networks
  nets <- d$nets
  
  # Checking balance: If the sampling heavily unbalanced, then return NAs
  prop <- lapply(nets, `diag<-`, NA)
  prop <- unlist(prop, recursive = TRUE)
  prop <- sum(prop == 1, na.rm = TRUE)/sum(!is.na(prop))
  
  # Else we estimate the lergm
  estimates <- if (!boot) 
    tryCatch(ergmito(nets ~ edges + mutual, ntries = 1L), error = function(e) e)
  else
    ergmito_boot(nets ~ edges + mutual, ncpus = 1L, R=1000)
  
  if (inherits(estimates, "error"))
    return(estimates)
  

  list(
    coef      = coef(estimates),
    vcov      = confint(estimates),
    balance   = prop,
    optim.out = estimates$optim.out
  )
    
    
}
