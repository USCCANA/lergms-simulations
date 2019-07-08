# Sampler
sampler_3_5 <- readRDS("data/sampler_3_5.rds")

# Function to simulate networks
simfun <- function(size, par, sampler) {
  
  # Simulating networks
  nets <- NULL
  for (i in 1:3)
    if (size[i] > 0)
      nets <- c(nets, sampler$sample(size[i], 2 + i, theta = par))
    
  nets
}

set.seed(112)
nsim   <- 1.5e5

# Simulating parameters: Scenario A --------------------------------------------
params_4 <- lapply(1:nsim, function(i) runif(2, min = .1, max = 2)*sample(c(-1,1), 2, TRUE))
sizes_4  <- lapply(1:nsim, function(i) c(0, rpois(1, 30), 0))


# Simulating parameters: Scenario B --------------------------------------------

params_3_5 <- lapply(1:nsim, function(i) runif(2, min = .1, max = 2)*sample(c(-1,1), 2, TRUE))
sizes_3_5  <- lapply(1:nsim, function(i) rpois(3, 10))

# Putting all together ---------------------------------------------------------

library(sluRm)
opts_sluRm$set_chdir("/staging/ggv")
opts_sluRm$set_job_name("ergmito-dgp")
opts_sluRm$set_opts(time = "04:00:00") #, account="lc_pdt", partition="thomas")
opts_sluRm$verbose_on()

# opts_sluRm$set_opts(account="lc_pdt", partition="thomas")

dgp_4 <- Slurm_Map(function(p, s) {
      list(par = p, size = s, nets = simfun(s, p, sampler_3_5))
   },
   p        = params_4,
   s        = sizes_4,
   njobs    = 100,
   mc.cores = 4,
   export   = c("sampler_3_5", "simfun")
)

dgp_4 <- Slurm_collect(dgp_4)

dgp_3_5 <- Slurm_Map(function(p, s) {
     list(par = p, size = s, nets = simfun(s, p, sampler_3_5))
   },
   p        = params_3_5,
   s        = sizes_3_5,
   njobs    = 100,
   mc.cores = 4,
   export   = c("simfun", "sampler_3_5")
)

dgp_3_5 <- Slurm_collect(dgp_3_5)


saveRDS(dgp_4, "simulations/dgp_4.rds")
saveRDS(dgp_3_5, "simulations/dgp_3_5.rds")

