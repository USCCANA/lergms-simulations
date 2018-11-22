simfun <- function(d, sampler) {
  
  # Simulating networks
  nets <- NULL
  for (i in 1:3)
    if (d$size[i] > 0)
      nets <- c(nets, sampler$sample(d$size[i], 2 + i, theta = d$par))
  
  # Checking balance: If the sampling heavily unbalanced, then return NAs
  prop <- lapply(nets, `diag<-`, NA)
  prop <- unlist(prop, recursive = TRUE)
  prop <- sum(prop == 1, na.rm = TRUE)*sum(prop == 0, na.rm = TRUE)/length(prop)^2
  
  if (prop < .1) {
    null <- d$par
    null[] <- rep(NA, length(null))
    return(
      list(
        coef    = null,
        vcov    = matrix(NA, nrow = length(null), ncol = length(null),
                      dimnames = list(names(null), names(null))),
        balance = prop
      )
    )
  }
    
  # Else we estimate the lergm
  estimates <- lergm(nets ~ edges + mutual)
  
  list(
    coef    = coef(estimates),
    vcov    = vcov(estimates),
    balance = prop
  )
    
    
}
