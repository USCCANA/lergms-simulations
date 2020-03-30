# library(ergmito)
# library(ergm)

fitter <- function(model, d, boot = FALSE) {
  
  # Simulating networks
  nets <- d$nets
  
  # Setting up the model
  model <- update.formula(model, nets ~ .)
  environment(model) <- environment()

  time_mle <- system.time(ans_mle <- if (!boot) 
    tryCatch(ergmito(model, ntries = 1L), error = function(e) e)
  else
    ergmito_boot(model, ncpus = 1L, R=1000)
  )

  # Getting the blockdiagonal version of the model and fixing formulas
  nets_bd  <- blockdiagonalize(nets)
  model_bd <- update.formula(model, nets_bd ~ .)
  environment(model_bd) <- environment()
  
  time_mcmle <- system.time(ans_mcmle <- tryCatch(ergm(
    model_bd,
    constraints = ~ blockdiag("block"),
    control     = control.ergm(
      # Default values equal to 1048
      MCMC.samplesize = 1024L * 4L,
      MCMC.interval   = 1024L * 4L,
      MCMC.burnin     = 1024L * 8L,
      seed            = 1L
      )
    ), error = function(e) e))
  
  time_rm <- system.time(ans_rm <- tryCatch(ergm(
    model_bd,
    constraints = ~ blockdiag("block"),
    control     = control.ergm(
      # Default values equal to 1048
      # MCMC.samplesize = 2048L,
      # MCMC.interval   = 2048,
      main.method     = "Robbins-Monro",
      MCMC.samplesize = 1024L * 4L,
      MCMC.interval   = 1024L * 4L,
      MCMC.burnin     = 1024L * 8L,
      seed            = 1L
    )
  ), error = function(e) e))
  
  if (inherits(ans_mle, "ergmito")) {
    estimates_mle <- list(
      coef       = coef(ans_mle),
      ci         = confint(ans_mle),
      vcov       = vcov(ans_mle),
      status     = ans_mle$status,
      ll         = logLik(ans_mle),
      time       = time_mle
    )
  } else estimates_mle <- ans_mle

  if (inherits(ans_mcmle, "ergm")) {
    estimates_mcmle <- list(
      coef = coef(ans_mcmle),
      ci   = confint(ans_mcmle),
      vcov = vcov(ans_mcmle),
      degeneracy = any(is.infinite(
        ergm.degeneracy(ans_mcmle)$degeneracy.value)
      ),
      ll   = logLik(ans_mcmle),
      time = time_mcmle
    )
  } else
    estimates_mcmle <- ans_mcmle
  
  if (inherits(ans_rm, "ergm")) {
    estimates_ergm <- list(
      coef = coef(ans_rm),
      ci   = confint(ans_rm),
      vcov = vcov(ans_rm),
      degeneracy = any(is.infinite(
        ergm.degeneracy(ans_rm)$degeneracy.value)
      ),
      ll   = logLik(ans_rm),
      time = time_rm
    )
  } else
    estimates_rm <- ans_rm
  
  list(
    mle = estimates_mle,
    mcmle    = estimates_mcmle,
    rm = estimates_rm
  )
  
}

