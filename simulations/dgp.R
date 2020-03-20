
# Sampler
sampler_3_5_ttriad <- readRDS("data/sampler_3_5_ttriad.rds")
sampler_3_5_mutual <- readRDS("data/sampler_3_5_mutual.rds")

# Function to simulate networks
simfun <- function(size, par, sampler) {
  
  # Simulating networks
  nets <- NULL
  nets <- c(nets, sampler$sample(size[1], 4, theta = par))
  nets <- c(nets, sampler$sample(size[2], 5, theta = par))
   
  nets
}

set.seed(112)
nsim   <- 2e4

# Simulating -------------------------------------------------------------------

# Reducing the sequence of possible values
U <- seq(from = .1, to = 2, by = .05)
U <- c(rev(-U), U)

params_3_5 <- lapply(1:nsim, function(i) sample(U, 2, TRUE))


sizes <- c(5, 10, 30, 50, 100, 150, 200, 300)
nsizes <- length(sizes)
S <- vector("list", nsizes)
for (i in seq_along(S)) {
   S[[i]] <- sample.int(sizes[i], nsim/nsizes, TRUE)
   S[[i]] <- cbind(S[[i]], sizes[i] - S[[i]])
}

sizes_4_5  <- do.call(rbind, S)
sizes_4_5 <- lapply(seq_len(nsim), function(i )sizes_4_5[i, ])

# Putting all together ---------------------------------------------------------

library(slurmR)
opts_slurmR$set_tmp_path("/staging/ggv")
opts_slurmR$set_job_name("ergmito-dgp")
opts_slurmR$set_opts(time = "04:00:00", account="lc_dvc", partition="conti")
opts_slurmR$verbose_on()

opts_slurmR$set_opts(account="lc_pdt", partition="thomas")

# Mutual model
dgp_4_5_mutual <- Slurm_Map(
   function(p, s) {
     list(par = p, size = s, nets = simfun(s, p, sampler_3_5_mutual))
   },
   p        = params_3_5,
   s        = sizes_4_5,
   njobs    = 200,
   mc.cores = 1L,
   export   = c("simfun", "sampler_3_5_mutual"),
   plan     = "wait",
   job_name = "ergmito-dgp-mutual"
)

Sys.sleep(30)


dgp_4_5_ttriad <- Slurm_Map(
   function(p, s) {
      list(par = p, size = s, nets = simfun(s, p, sampler_3_5_ttriad))
      },
   p        = params_3_5,
   s        = sizes_4_5,
   njobs    = 200,
   mc.cores = 1L,
   export   = c("simfun", "sampler_3_5_ttriad"),
   plan     = "wait",
   job_name = "ergmito-dgp-ttriad"
)


Sys.sleep(30)


# Transitive triads model
dgp_4_5_ttriad <- Slurm_collect(dgp_4_5_ttriad)
dgp_4_5_mutual <- Slurm_collect(dgp_4_5_mutual)
saveRDS(dgp_4_5_mutual, "simulations/dgp_4_5_mutual.rds")
saveRDS(dgp_4_5_ttriad, "simulations/dgp_4_5_ttriad.rds")

