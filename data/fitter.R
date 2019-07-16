# library(ergmito)
# library(ergm)

fitter <- function(model, d, boot = FALSE) {
  
  # Simulating networks
  nets <- d$nets
  
  # Checking balance: If the sampling heavily unbalanced, then return NAs
  prop <- lapply(nets, `diag<-`, NA)
  prop <- unlist(prop, recursive = TRUE)
  prop <- sum(prop == 1, na.rm = TRUE)/sum(!is.na(prop))
  
  # Estimating the ERGM
  model <- update.formula(model, nets ~ .)
  environment(model) <- environment()
  ans_ergmito <- if (!boot) 
    tryCatch(ergmito(model, ntries = 1L), error = function(e) e)
  else
    ergmito_boot(model, ncpus = 1L, R=1000)
  
  # if (inherits(ans_ergmito, "error"))
  #   return(ans_ergmito)

  # Getting the blockdiagonal version of the model
  nets_bd <- blockdiagonalize(nets)
  model <- update.formula(model, nets_bd ~ .)
  environment(model) <- environment()
  ans_ergm <- tryCatch(ergm(
    model,
    constraints = ~ blockdiag("block"),
    control     = control.ergm(
      # Default values equal to 1048
      MCMC.samplesize = 2048L,
      MCMC.interval   = 2048L
      )
    
    ), error = function(e) e)
  

  # Computing loglikelihood under each model
  llfun <- function(p) {
    with(ans_ergmito$formulae, {
      loglik(p, stats.weights = stats.weights, stats.statmat = stats.statmat,
             target.stats = target.stats)
    })
  }
  
  estimates_ergmito <- list(
    coef       = coef(ans_ergmito),
    ci         = confint(ans_ergmito),
    vcov       = vcov(ans_ergmito),
    ll         = llfun(coef(ans_ergmito)),
    balance    = prop,
    optim.out  = ans_ergmito$optim.out,
    degeneracy = ans_ergmito$degeneracy
  )

  if (inherits(ans_ergm, "ergm")) {
    estimates_ergm <- list(
      coef = coef(ans_ergm),
      ci   = confint(ans_ergm),
      vcov = vcov(ans_ergm),
      ll   = llfun(coef(ans_ergm)),
      degeneracy = any(is.infinite(
        ergm.degeneracy(ans_ergm)$degeneracy.value)
      )
    )
  } else
    estimates_ergm <- ans_ergm
  
  list(
    ergmito    = estimates_ergmito,
    ergm       = estimates_ergm
  )
    
    
}
