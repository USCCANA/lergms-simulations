library(ergmito)
library(ergm)
library(parallel)
library(sluRm)

# Loading simulation function and data
source("data/fitter.R")

# Transitive model -------------------------------------------------------------
dat <- readRDS("simulations/dgp_4_5_null.rds")

# Creating cluster
opts_sluRm$set_tmp_path("/staging/ggv/")
opts_sluRm$set_job_name("03-various-sizes-4-5-null")
opts_sluRm$set_opts(
  account = "lc_dvc",
  partition="conti",
  time="04:00:00", `mem-per-cpu` = "1G"
)

# Checking veb
opts_sluRm$verbose_on()

job2 <- Slurm_lapply(
   dat, fitter, njobs = 200, mc.cores = 1L, plan = "wait",
   model = ~ edges + ttriad
   )

cat("~~ THE END ttriad ... COLLECTING ~~")

# Waiting just in case
Sys.sleep(60*2)

# ans1 <- Slurm_collect(job1)
ans2 <- Slurm_collect(job2)
# saveRDS(ans1, "simulations/02-various-sizes-4-5-mutual.rds", compress = FALSE)
saveRDS(ans2, "simulations/03-various-sizes-4-5-null.rds", compress = FALSE)

cat("~~ THE END ALL ~~")

