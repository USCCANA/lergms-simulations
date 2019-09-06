library(ergmito)
library(parallel)

set.seed(11)
RNGversion("3.4.0")
nsamples <- 20
nets <- lapply(1:nsamples, function(i) {
  rbernoulli(rep(5, 30), .5)
})


estimates <- mclapply(nets, function(n) ergmito(n ~ edges + mutual),
                      mc.cores = 4L)

summaries <- lapply(estimates, summary)

gofs      <- lapply(estimates, gof_ergmito)
