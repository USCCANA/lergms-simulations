library(ergmito)

# Sampler
sampler_3_5_mutual <- new_rergmito(rbernoulli(4) ~ edges + mutual, sizes = 3:5, mc.cores = 1L)

# Sampler
sampler_3_5_ttriad <- new_rergmito(rbernoulli(4) ~ edges + ttriad, sizes = 3:5, mc.cores = 1L)

saveRDS(object = sampler_3_5_ttriad, file = "data/sampler_3_5_ttriad.rds", compress=FALSE)
saveRDS(object = sampler_3_5_mutual, file = "data/sampler_3_5_mutual.rds", compress=FALSE)
