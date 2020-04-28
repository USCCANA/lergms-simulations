# library(ergmito)
# library(ergm)

fitter <- function(model, d, boot = FALSE, include = c(mle = TRUE, mcmle = TRUE, rm = TRUE)) {
  
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
      MCMC.samplesize = 1024L * 2L,
      MCMC.interval   = 1024L * 2L,
      MCMC.burnin     = 1024L * 32L,
      seed            = 1L
      )
    ), error = function(e) e))
  
  # We give it another chance if it was not able to fit it right
  time_mcmle0 <- NULL
  if (inherits(ans_mcmle, "error")) {
    
    time_mcmle0 <- time_mcmle
    
    time_mcmle  <- system.time(ans_mcmle <- tryCatch(ergm(
      model_bd,
      constraints = ~ blockdiag("block"),
      control     = control.ergm(
        # Default values equal to 1048
        MCMC.samplesize = 1024L * 10L,
        MCMC.interval   = 1024L * 10L,
        MCMC.burnin     = 1024L * 64L,
        seed            = 222L
        )
      ), error = function(e) e))
  }
   
  time_rm <- system.time(ans_rm <- tryCatch(ergm(
    model_bd,
    constraints = ~ blockdiag("block"),
    control     = control.ergm(
      # Default values equal to 1048
      # MCMC.samplesize = 2048L,
      # MCMC.interval   = 2048,
      main.method     = "Robbins-Monro",
      MCMC.samplesize = 1024L * 2L,
      MCMC.interval   = 1024L * 2L,
      MCMC.burnin     = 1024L * 32L,
      # Specifics for RM
      RM.phase1n_base = 7,
      RM.phase2n_base = 100,
      RM.phase2sub    = 7,
      RM.init_gain    = 0.5,
      RM.phase3n      = 500,
      seed            = 1L
    )
  ), error = function(e) e))
 
  time_rm0 <- NULL
  if (inherits(ans_rm, "error")) {
    
    time_rm0 <- time_rm
    
    time_rm  <- system.time(ans_rm <- tryCatch(ergm(
      model_bd,
      constraints = ~ blockdiag("block"),
      control     = control.ergm(
        # Default values equal to 1048
        main.method     = "Robbins-Monro",
        MCMC.samplesize = 1024L * 10L,
        MCMC.interval   = 1024L * 10L,
        MCMC.burnin     = 1024L * 64L,
        # Specifics for RM
        RM.phase1n_base = 7 + 6, # M1 in lusher, koskinen, robins
        RM.phase2n_base = 100,   
        RM.phase2sub    = 7 + 6, # In PNet they take the average
        RM.init_gain    = 0.1,   # a in lusher, koskinen, robins
        RM.phase3n      = 1000L,
        seed            = 222L
      )
    ), error = function(e) e))
  }
 
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
      time  = time_mcmle,
      time0 = time_mcmle0
    )
  } else
    estimates_mcmle <- ans_mcmle
  
  if (inherits(ans_rm, "ergm")) {
    estimates_rm <- list(
      coef  = coef(ans_rm),
      ci    = confint(ans_rm),
      vcov  = vcov(ans_rm),
      degeneracy = any(is.infinite(
        ergm.degeneracy(ans_rm)$degeneracy.value)
      ),
      ll    = logLik(ans_rm),
      time  = time_rm,
      time0 = time_rm0
    )
  } else
    estimates_rm <- ans_rm
 
  if (exists("niter_", envir = .GlobalEnv)) {
    niter_ <<- niter_ + 1L
  } else
    niter_ <<- 1L
  
  # Pasting iteration number
  message(
    paste(rep("#", options()$width), collapse=""),
    "\nIteration number: ",
    niter_, "\n",
    paste(rep("#", options()$width), collapse="")
    )

  list(
    mle   = estimates_mle,
    mcmle = estimates_mcmle,
    rm    = estimates_rm
  )
  
}


