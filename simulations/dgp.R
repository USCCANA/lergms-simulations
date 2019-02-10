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
nsim   <- 1e4

# Simulating parameters: Scenario A --------------------------------------------
params_4 <- lapply(1:nsim, function(i) runif(2, min = -2, max = 2))
sizes_4  <- lapply(1:nsim, function(i) c(0, rpois(1, 30), 0))


# Simulating parameters: Scenario B --------------------------------------------

params_3_5 <- lapply(1:nsim, function(i) runif(2, min = -2, max = 2))
sizes_3_5  <- lapply(1:nsim, function(i) rpois(3, 10))

# Putting all together ---------------------------------------------------------

#system("free -h")

#cl <- parallel::makeForkCluster(2L)

#system("free -h")

i <- 0
dgp_4 <- #parallel::clusterMap(cl, function(p,s) {
  Map(function(p,s) {
  message("OK ", (i <<- i + 1))
  list(par = p, size = s, nets = simfun(s, p, sampler_3_5))
}, p = params_4, s = sizes_4)

i <- 0
dgp_3_5 <- #parallel::clusterMap(cl, function(p,s) {
Map(function(p,s) {
  message("OK ", (i <<- i + 1))
  list(par = p, size = s, nets = simfun(s, p, sampler_3_5))
}, p = params_3_5, s = sizes_3_5)

#parallel::stopCluster(cl)

saveRDS(dgp_4, "simulations/dgp_4.rds")
saveRDS(dgp_3_5, "simulations/dgp_3_5.rds")
