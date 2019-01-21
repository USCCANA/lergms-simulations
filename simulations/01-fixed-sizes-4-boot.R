library(ergmito)
library(parallel)
library(sluRm)

# Loading simulation function
source("data/simfun.R")

# Loading sampler
# sampler_3_4 <- readRDS("data/sampler_3_4.rds")

# Simulating parameters
# set.seed(112)
# nsim   <- 2e3

# params <- lapply(1:nsim, function(i) runif(2, min = -3, max = 3))
# sizes  <- lapply(1:nsim, function(i) c(0, rpois(1, 40), 0))

# Putting all together
dat <- readRDS("simulations/dgp_4.rds") # Map(function(p,s) {list(par = p, size = s)}, p = params, s = sizes)

# Creating cluster
opts_sluRm$set_chdir("/staging/ggv/")
opts_sluRm$set_job_name("01-fixed-sizes-4-boot")
opts_sluRm$set_opts(account = "lc_pdt", partition="thomas", time="08:00:00")

job <- Slurm_lapply(dat, simfun, njobs = 45, mc.cores = 4L, boot=TRUE)
ans <- Slurm_collect(job)

saveRDS(ans, "simulations/01-fixed-sizes-4-boot.rds", compress = FALSE)
# saveRDS(dat, "simulations/01-fixed-sizes-4-dat.rds", compress = FALSE)


# 
# 
# # Computing biases
# coef_estimates <- lapply(ans, "[[", "coef")
# coef_estimates <- do.call(rbind, coef_estimates)
# 
# bias <- coef_estimates - do.call(rbind, params)
# 
# bias <- data.frame(
#   bias = c(bias[,1], bias[,2]),
#   par  = c(rep("edges", nsim), rep("mutual", nsim))
# )
# 
# library(ggplot2)
# ggplot(bias, aes(x = par, y = bias)) +
#   geom_violin() + 
#   labs(
#     title     = "Distribution of Empirical Bias",
#     subtitle = "a"
#   )
# 

