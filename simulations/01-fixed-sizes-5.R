library(lergm)
library(parallel)
library(sluRm)

# Loading simulation function
source("data/simfun.R")

# Loading sampler
sampler <- readRDS("data/sampler_3_5.rds")

# Simulating parameters
set.seed(112)
nsim   <- 1e3

params <- lapply(1:nsim, function(i) runif(2, min = -5, max = 5))
sizes  <- lapply(1:nsim, function(i) rpois(3, 10))

# Putting all together
dat <- Map(function(p,s) {list(par = p, size = s)}, p = params, s = sizes)

# Creating cluster
opts_ 
ans <- Slurm_lapply(cl, dat, simfun)
stopCluster(cl)

# Computing biases
coef_estimates <- lapply(ans, "[[", "coef")
coef_estimates <- do.call(rbind, coef_estimates)

bias <- coef_estimates - do.call(rbind, params)

bias <- data.frame(
  bias = c(bias[,1], bias[,2]),
  par  = c(rep("edges", nsim), rep("mutual", nsim))
)

library(ggplot2)
ggplot(bias, aes(x = par, y = bias)) +
  geom_violin() + 
  labs(
    title     = "Distribution of Empirical Bias",
    subtitle = "a"
  )

