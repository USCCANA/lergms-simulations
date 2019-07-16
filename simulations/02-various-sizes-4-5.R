library(ergmito)
library(ergm)
library(parallel)
library(sluRm)

# Loading simulation function and data
source("data/fitter.R")

# Mutual model -----------------------------------------------------------------
dat <- readRDS("simulations/dgp_4_5_mutual.rds")

# Creating cluster
opts_sluRm$set_tmp_path("/staging/ggv/")
opts_sluRm$set_job_name("02-various-sizes-4-5-mutual")
opts_sluRm$set_opts(
   account = "lc_dvc",
   partition="conti",
   time="04:00:00", `mem-per-cpu` = "1G"
)

# Checking veb
opts_sluRm$verbose_on()

job <- Slurm_lapply(dat, fitter, njobs = 200, mc.cores = 1L, plan = "wait", model = ~ edges + mutual)
ans <- Slurm_collect(job)

saveRDS(ans, "simulations/02-various-sizes-4-5-mutual.rds", compress = FALSE)

cat("~~ THE END mutual ~~")

# Transitive model -------------------------------------------------------------
dat <- readRDS("simulations/dgp_4_5_ttriad.rds")

# Creating cluster
opts_sluRm$set_tmp_path("/staging/ggv/")
opts_sluRm$set_job_name("02-various-sizes-4-5-ttriad")
opts_sluRm$set_opts(
  account = "lc_dvc",
  partition="conti",
  time="04:00:00", `mem-per-cpu` = "1G"
)

# Checking veb
opts_sluRm$verbose_on()

job <- Slurm_lapply(dat, fitter, njobs = 200, mc.cores = 1L, plan = "wait", model = ~ edges + ttriad)
ans <- Slurm_collect(job)

saveRDS(ans, "simulations/02-various-sizes-4-5-ttriad.rds", compress = FALSE)

cat("~~ THE END ttriad ~~")
