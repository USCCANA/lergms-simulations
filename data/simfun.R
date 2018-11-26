simfun <- function(d, sampler, boot = FALSE) {
  
  # Simulating networks
  nets <- NULL
  for (i in 1:3)
    if (d$size[i] > 0)
      nets <- c(nets, sampler$sample(d$size[i], 2 + i, theta = d$par))
  
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
    lergm(nets ~ edges + mutual)
  else
    lergm_boot(nets ~ edges + mutual, ncpus = 1L, R=1000)
  
  list(
    coef    = coef(estimates),
    vcov    = confint(estimates),
    balance = prop
  )
    
    
}
