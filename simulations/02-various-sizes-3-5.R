library(ergmito)
library(ergm)
library(parallel)
library(sluRm)

# Loading simulation function and data
source("data/fitter.R")
dat <- readRDS("simulations/dgp_3_5.rds")

# Creating cluster
opts_sluRm$set_tmp_path("/staging/ggv/")
opts_sluRm$set_job_name("02-various-sizes-3-5")
opts_sluRm$set_opts(
   account = "lc_dvc",
   partition="conti",
   time="04:00:00", `mem-per-cpu` = "1G"
)

# Checking veb
opts_sluRm$verbose_on()

job <- Slurm_lapply(dat, fitter, njobs = 200, mc.cores = 1L, plan = "wait")
ans <- Slurm_collect(job)

saveRDS(ans, "simulations/02-various-sizes-3-5.rds", compress = FALSE)

cat("~~ THE END ~~")
