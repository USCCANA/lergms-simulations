# library(ergmito)
# library(ergm)

fitter <- function(d, boot = FALSE) {
  
  # Simulating networks
  nets <- d$nets
  
  # Checking balance: If the sampling heavily unbalanced, then return NAs
  prop <- lapply(nets, `diag<-`, NA)
  prop <- unlist(prop, recursive = TRUE)
  prop <- sum(prop == 1, na.rm = TRUE)/sum(!is.na(prop))
  
  # Estimating the ERGM
  estimates <- if (!boot) 
    tryCatch(ergmito(nets ~ edges + mutual, ntries = 1L), error = function(e) e)
  else
    ergmito_boot(nets ~ edges + mutual, ncpus = 1L, R=1000)
  
  if (inherits(estimates, "error"))
    return(estimates)

  # Getting the blockdiagonal version of the model
  nets_bd <- blockdiagonalize(nets)
  estimates_ergm <- ergm(nets_bd ~ edges + mutual, constrains = ~ blockdiag("block"))
  

  estimates_ergmito <- list(
    coef       = coef(estimates),
    ci         = confint(estimates),
    vcov       = vcov(estimates),
    balance    = prop,
    optim.out  = estimates$optim.out,
    degeneracy = estimates$degeneracy
  )

  estimates_ergm <- list(
    coef = coef(estimates_ergm),
    vcov = vcov(estimates_ergm),
    ci   = confint(estimates_ergm)
  )

  list(
    ergmito = estimates_ergmito,
    ergm    = estimates_ergm
  )
    
    
}
