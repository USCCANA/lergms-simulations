library(ergmito)
library(parallel)
library(sluRm)

# Loading simulation function and data
source("data/simfun.R")
dat <- readRDS("simulations/dgp_4.rds")

# Creating cluster
opts_sluRm$set_chdir("/staging/ggv/")
opts_sluRm$set_job_name("01-fixed-sizes-4")
opts_sluRm$set_opts(account = "lc_pdt", partition="thomas")

job <- Slurm_lapply(dat, simfun, njobs = 20, mc.cores = 4L)
ans <- Slurm_collect(job)

saveRDS(ans, "simulations/01-fixed-sizes-4.rds", compress = FALSE)

