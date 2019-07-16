
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
nsim   <- 1e3

# Simulating -------------------------------------------------------------------

# Reducing the sequence of possible values
U <- seq(from = .1, to = 2, by = .05)
U <- c(rev(-U), U)

params_3_5 <- lapply(1:nsim, function(i) sample(U, 2, TRUE))


sizes <- c(10, 20, 30, 40, 50)
nsizes <- length(sizes)
S <- vector("list", nsizes)
for (i in seq_along(S)) {
   S[[i]] <- sample.int(sizes[i], nsim/nsizes, TRUE)
   S[[i]] <- cbind(S[[i]], sizes[i] - S[[i]])
}

sizes_4_5  <- do.call(rbind, S)
sizes_4_5 <- lapply(seq_len(nsim), function(i )sizes_4_5[i, ])

# Putting all together ---------------------------------------------------------

library(sluRm)
opts_sluRm$set_tmp_path("/staging/ggv")
opts_sluRm$set_job_name("ergmito-dgp")
opts_sluRm$set_opts(time = "04:00:00", account="lc_dvc", partition="conti")
opts_sluRm$verbose_on()

opts_sluRm$set_opts(account="lc_pdt", partition="thomas")

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

dgp_4_5_mutual <- Slurm_collect(dgp_4_5_mutual)

# Transitive triads model
saveRDS(dgp_4_5_mutual, "simulations/dgp_4_5_mutual.rds")

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

dgp_4_5_ttriad <- Slurm_collect(dgp_4_5_ttriad)


saveRDS(dgp_4_5_ttriad, "simulations/dgp_4_5_ttriad.rds")

