library(ergmito)
library(parallel)
library(sluRm)

# Loading simulation function and data
source("data/simfun.R")
dat <- readRDS("simulations/dgp_3_5.rds")

# Creating cluster
opts_sluRm$set_chdir("/staging/ggv/")
opts_sluRm$set_job_name("02-various-sizes-3-5")
opts_sluRm$set_opts(
   # account = "lc_pdt",
   # partition="thomas",
   time="04:00:00", `mem-per-cpu` = "2G")

job <- Slurm_lapply(dat, simfun, njobs = 10, mc.cores = 8L)
ans <- Slurm_collect(job)

saveRDS(ans, "simulations/02-various-sizes-3-5.rds", compress = FALSE)
