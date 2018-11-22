simfun <- function(d, sampler) {
  
  # Simulating networks
  nets <- NULL
  for (i in 1:3)
    if (d$size[i] > 0)
      nets <- c(nets, sampler$sample(d$size[i], 2 + i, theta = d$par))
    
    estimates <- lergm(nets ~ edges + mutual)
    
    list(
      coef = coef(estimates),
      vcov = vcov(estimates)
    )
    
    
}
