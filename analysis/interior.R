library(ergmito)
library(data.table)
library(parallel)
library(magrittr)

dgp <- readRDS("simulations/dgp_4_5_ttriad.rds") %>%
  lapply("[[", "nets")
dat <- readRDS("simulations/02-various-sizes-4-5-ttriad.rds")

# Found by ERGM
fitted_mcmle <- sapply(dat, function(r) {
  if (inherits(r$ergm, "error"))
    return(FALSE)
  !r$ergm$degeneracy
}) %>% which

fitted_mle <- sapply(dat, function(r) {
  if (inherits(r$ergmito, "error"))
    return(FALSE)
  r$ergmito$status == 0L
}) %>% which

fitted_common <- intersect(fitted_mcmle, fitted_mle)
nfitted <- length(fitted_common)

# Looking at which statistics are not in the interior of the set of
# sufficient statistics

# Generating ranges (max)
mat4 <- matrix(1, ncol = 4, nrow = 4)
diag(mat4) <- 0
mat5 <- matrix(1, ncol = 5, nrow = 5)
diag(mat5) <- 0

maxref <- matrix(ncol=2, nrow=5)
maxref[4,] <- count_stats(mat4 ~ edges + ttriad)
maxref[5,] <- count_stats(mat5 ~ edges + ttriad)


stats <- mclapply(dgp[fitted_common], count_stats, terms = c("edges", "ttriad"), mc.cores=4L)
sizes <- mclapply(dgp[fitted_common], nvertex, mc.cores=4L)
stats <- mcMap(function(x, s) {
  
  # Normalizing to the size
  colMeans(x/maxref[s,])
  
}, x = stats, s = sizes, mc.cores=4L)
stats <- do.call(rbind, stats)
stats <- as.data.table(stats)
colnames(stats) <- c("edges", "ttriad")

View(data.frame(table(stats$edges)), "Stats")

stats_df <- data.frame(
  count = as.vector(stats),
  term  = c(rep("edges", nrow(stats)), rep("ttriad", nrow(stats)))
)
