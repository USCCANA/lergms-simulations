library(ergmito)

# Sampler
sampler_3_5 <- new_rergmito(rbernoulli(4) ~ edges + mutual, sizes = 3:5, mc.cores = 1L)

saveRDS(object = sampler_3_5, file = "data/sampler_3_5.rds", compress=FALSE)
