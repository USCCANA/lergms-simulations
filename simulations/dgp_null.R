#!/bin/sh
#SBATCH --partition=thomas
#SBATCH --account=lc_pdt
#SBATCH --time=10:00:00
#SBATCH --mem=16gb
#SBATCH --mail-user=g.vegayon@gmail.com
#SBATCH --mail-type=ALL

# Sampler
library(ergmito)
set.seed(12)
data(fivenets)
sampler_3_5_edges <- new_rergmito(fivenets ~ edges, sizes = 3:5, mc.cores = 1L)

# Function to simulate networks
simfun <- function(size, par, sampler) {
  
  # Simulating networks
  nets <- NULL
  nets <- c(nets, sampler$sample(size[1], 4, theta = par))
  nets <- c(nets, sampler$sample(size[2], 5, theta = par))
  
  nets
}

set.seed(112)
nsim   <- 35000

# Simulating -------------------------------------------------------------------

# Reducing the sequence of possible values
U <- seq(from = .1, to = 2, by = .05)
U <- c(rev(-U), U)

params_3_5 <- lapply(1:nsim, function(i) sample(U, 1, TRUE))

sizes <- c(5, 10, 15, 20, 30, 50, 100)
nsizes <- length(sizes)
S <- vector("list", nsizes)
for (i in seq_along(S)) {
  S[[i]] <- sample.int(sizes[i], nsim/nsizes, TRUE)
  S[[i]] <- cbind(S[[i]], sizes[i] - S[[i]])
}

sizes_4_5  <- do.call(rbind, S)
sizes_4_5 <- lapply(seq_len(nsim), function(i) sizes_4_5[i, ])

# Putting all together ---------------------------------------------------------

library(slurmR)
opts_slurmR$set_tmp_path("/staging/ggv")
opts_slurmR$set_job_name("ergmito-dgp-null")
opts_slurmR$set_opts(time = "08:00:00", account="lc_dvc", partition="conti")
opts_slurmR$verbose_on()

opts_slurmR$set_opts(account="lc_ggv", partition="scavenge")

# Mutual model
dgp_4_5_null <- Slurm_Map(
  function(p, s) {
    list(par = p, size = s, nets = simfun(s, p, sampler_3_5_edges))
  },
  p        = params_3_5,
  s        = sizes_4_5,
  njobs    = 400,
  mc.cores = 1L,
  export   = c("simfun", "sampler_3_5_edges"),
  plan     = "wait",
  job_name = "ergmito-dgp-null-map",
  sbatch_opt = list(`mem-per-cpu` = "2gb")
)


Sys.sleep(30)


# Transitive triads model
dgp_4_5_null <- Slurm_collect(dgp_4_5_null)
saveRDS(dgp_4_5_null, "simulations/dgp_4_5_null-larger.rds")

