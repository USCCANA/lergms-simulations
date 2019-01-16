library(ggplot2)
library(lergm)

experiments <- c(
  "Distribution of Empirical Bias by parameter (Networks of size 4)" = "01-fixed-sizes-4",
  "Distribution of Empirical Bias by parameter (Networks of size 3 and 4)" = "02-various-sizes-3-4"
)

sampler <- readRDS("data/sampler_3_4.rds")

simfun <- function(d, sampler) {
  
  # Simulating networks
  nets <- NULL
  for (i in 1:3)
    if (d$size[i] > 0)
      nets <- c(nets, sampler$sample(d$size[i], 2 + i, theta = d$par))
  
  nets
}

for (i in seq_along(experiments)) {
  
  e <- experiments[i]
  
  # Reading data
  res <- readRDS(sprintf("simulations/%s.rds", e))
  dgp <- readRDS(sprintf("simulations/%s-dat.rds", e))
  
  parallel::mclapply()
  
  # Simulating networks
  nets <- NULL
  for (i in 1:3)
    if (d$size[i] > 0)
      nets <- c(nets, sampler$sample(d$size[i], 2 + i, theta = d$par))
  
}