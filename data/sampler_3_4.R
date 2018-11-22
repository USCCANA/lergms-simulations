library(lergm)

# Sampler
sampler_3_4 <- new_rlergm(rbernoulli(4) ~ edges + mutual, sizes = 2:4)

saveRDS(object = sampler_3_4, file = "data/sampler_3_4.rds", compress=FALSE)