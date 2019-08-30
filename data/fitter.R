# library(ergmito)
# library(ergm)

fitter <- function(model, d, boot = FALSE) {
  
  # Simulating networks
  nets <- d$nets
  
  # Setting up the model
  model <- update.formula(model, nets ~ .)
  environment(model) <- environment()

  time_ergmito <- system.time(ans_ergmito <- if (!boot) 
    tryCatch(ergmito(model, ntries = 1L), error = function(e) e)
  else
    ergmito_boot(model, ncpus = 1L, R=1000)
  )

  # Getting the blockdiagonal version of the model and fixing formulas
  nets_bd  <- blockdiagonalize(nets)
  model_bd <- update.formula(model, nets_bd ~ .)
  environment(model_bd) <- environment()
  
  time_ergm <- system.time(ans_ergm <- tryCatch(ergm(
    model_bd,
    constraints = ~ blockdiag("block"),
    control     = control.ergm(
      # Default values equal to 1048
      MCMC.samplesize = 2048L,
      MCMC.interval   = 2048,
      seed            = 1L
      )
    ), error = function(e) e))
  
  if (inherits(ans_ergmito, "ergmito")) {
    estimates_ergmito <- list(
      coef       = coef(ans_ergmito),
      ci         = confint(ans_ergmito),
      vcov       = vcov(ans_ergmito),
      status     = ans_ergmito$status,
      ll         = logLik(ans_ergmito),
      time       = time_ergmito
    )
  } else estimates_ergmito <- ans_ergmito

  if (inherits(ans_ergm, "ergm")) {
    estimates_ergm <- list(
      coef = coef(ans_ergm),
      ci   = confint(ans_ergm),
      vcov = vcov(ans_ergm),
      degeneracy = any(is.infinite(
        ergm.degeneracy(ans_ergm)$degeneracy.value)
      ),
      ll   = logLik(ans_ergm),
      time = time_ergm
    )
  } else
    estimates_ergm <- ans_ergm
  
  list(
    ergmito = estimates_ergmito,
    ergm    = estimates_ergm
  )
  
}

