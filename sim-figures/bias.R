library(ggplot2)
library(ggforce)
library(ggridges)
library(ergmito)
library(data.table)

source("sim-figures/interval_tags.R")

experiments <- c(
  # "Distribution of Empirical Bias (mutual)" = "02-various-sizes-4-5-mutual",
  "Distribution of Empirical Bias (ttriad)" = "02-various-sizes-4-5-ttriad"
)

# Colors to be used to fill the plots
fillcols <- c(MLE="white", "MC-MLE" = "gray", RM = "black")
fillcols[] <- adjustcolor(fillcols, alpha.f = .7) 

term_name <- c("edges", "ttriad")

# Reading data
res  <- readRDS("simulations/02-various-sizes-4-5-ttriad.rds") # readRDS(sprintf("simulations/%s.rds", e))
# res2 <- readRDS("simulations/02-various-sizes-4-5-mle-only.rds") # readRDS(sprintf("simulations/%s.rds", e))

# res <- lapply(res, `names<-`, c("mle", "mcmle"))

dgp  <- readRDS("simulations/02-various-sizes-4-5-ttriad-dat.rds")
pars <- lapply(dgp, "[[", "par")

sizes <- lapply(dgp, "[[", "size")
sizes <- sapply(sizes, sum)
sizes <- sort(unique(unlist(sizes)))

sizes_labs <- structure(paste(sizes, "nets"), names = sizes)

# Alternatively, we are defining the common set to that in which the graphs are
# not fully connected nor are empty
fitted_common <- sapply(dgp, function(d) {
  n <- nvertex(d$nets)
  d <- count_stats(d$nets ~ edges + ttriad)
  any(d[,2] > 0) & any(d[,1] < (n*(n-1)))
})

fitted_common <- which(fitted_common)
nfitted <- length(fitted_common)

# fitted_common2 <- intersect(
#   which(sapply(res, function(r) {
#     if (inherits(r$mcmle, "error"))
#       return(FALSE)
#     !r$mcmle$degeneracy
#   })),
#   which(sapply(res, function(r) {
#     if (inherits(r$rm, "error"))
#       return(FALSE)
#     !r$rm$degeneracy
#   }))
# )
# fitted_common2 <- intersect(
#   fitted_common2,
#   which(sapply(res, function(r) {
#     if (inherits(r$mle, "error"))
#       return(FALSE)
#     !r$mle$status
#   }))
# )
# nfitted<-length(fitted_common2)
# fitted_common <- fitted_common2

# Comparing: MCMLE vs MLE ------------------------------------------------------

# Computing bias
bias_mle <- lapply(res[fitted_common], function(d) {
  if (inherits(d$mle, "error"))
    return(c(edges = NA, ttriple = NA))
  d$mle$coef
})
bias_mle <- do.call(rbind, bias_mle) - do.call(rbind, pars[fitted_common])

bias_mcmle <- lapply(res[fitted_common], function(d) {
  if (inherits(d$mcmle, "error"))
    return(c(edges = NA, ttriple = NA))
  d$mcmle$coef
})
bias_mcmle <- do.call(rbind, bias_mcmle) - do.call(rbind, pars[fitted_common])

bias_rm <- lapply(res[fitted_common], function(d) {
  if (inherits(d$rm, "error"))
    return(c(edges = NA, ttriple = NA))
  d$rm$coef
})
bias_rm <- do.call(rbind, bias_rm) - do.call(rbind, pars[fitted_common])

# Side by side bias
dat <- data.frame(
  abs_bias = c(bias_mcmle[,1], bias_mle[,1], bias_rm[,1], bias_mcmle[,2], bias_mle[,2], bias_rm[,2]),
  term     = c(rep(term_name[1], nfitted*3), rep(term_name[2], nfitted*3)),
  model    = rep(c(rep("MC-MLE", nfitted), rep("MLE", nfitted), rep("RM", nfitted)), 2),
  size     = sapply(lapply(dgp[fitted_common], "[[", "size"), sum),
  stringsAsFactors = FALSE
)

dat$model <- factor(dat$model, levels = names(fillcols))

p <- ggplot(dat[is.finite(dat$abs_bias), ], aes(abs_bias, y=model)) + 
  theme_light() +
  scale_fill_manual(values = fillcols) +
  geom_density_ridges2(aes(fill=model)) +
  xlim(c(-2, 2)) +
  facet_grid(
    term ~ size,
    labeller = labeller(
      size = sizes_labs
    )) +
  coord_flip() +
  geom_vline(xintercept=0, lty=2) +
  xlab("Empirical Bias") +
  labs(fill = "Method") +
  ylab(NULL) +
  theme(
    axis.ticks.x  = element_blank(),
    axis.text.x  = element_blank(),
    text = element_text(family = "AvantGarde")
  ) 

p  +
  ggsave(
    "sim-figures/bias.pdf",
    width = 8*.8, height = 6*.8
  )

# Testing differences
t.test(bias_mcmle[,2], bias_mle[,2], alternative = "two.sided", paired = TRUE)
t.test(bias_rm[,2], bias_mle[,2], alternative = "two.sided", paired = TRUE)
t.test(bias_mle[,2], alternative = "two.sided")
t.test(bias_mcmle[,2], alternative = "two.sided")
t.test(bias_rm[,2], alternative = "two.sided")

# Time to get an answer
fitted_common <- intersect(
  which(sapply(res, function(r) !inherits(r$mle, "error"))),
  which(sapply(res, function(r) !inherits(r$mcmle, "error")))
)
fitted_common <- intersect(
  fitted_common,
  which(sapply(res, function(r) !inherits(r$rm, "error")))
)

times_mcmle <- lapply(res[fitted_common], "[[", "mcmle") 
times_mcmle <- lapply(times_mcmle, "[[", "time") 
times_mcmle <- do.call(rbind, times_mcmle) 
times_mcmle <- data.table(times_mcmle) 
times_mcmle <- times_mcmle[, Method := "MC-MLE"]

times_mle <- lapply(res[fitted_common], "[[", "mle") 
times_mle <- lapply(times_mle, "[[", "time") 
times_mle <- do.call(rbind, times_mle) 
times_mle <- data.table(times_mle) 
times_mle <- times_mle[, Method := "MLE"]

times_rm <- lapply(res[fitted_common], "[[", "rm") 
times_rm <- lapply(times_rm, "[[", "time") 
times_rm <- do.call(rbind, times_rm) 
times_rm <- data.table(times_rm) 
times_rm <- times_rm[, Method := "RM"]

times_mcmle[, Size        := sapply(lapply(dgp[fitted_common], "[[", "size"), sum)]
times_mcmle[, Relative    := elapsed/times_mle$elapsed]
times_rm[, Size        := sapply(lapply(dgp[fitted_common], "[[", "size"), sum)]
times_rm[, Relative    := elapsed/times_mle$elapsed]
times_mle[, Size     := sapply(lapply(dgp[fitted_common], "[[", "size"), sum)]
times_mle[, Relative := 1.0]

dat_times <- rbindlist(list(times_mle, times_mcmle, times_rm))
dat_times[, Method := factor(Method, levels = names(fillcols))]
  
ggplot(
  dat_times,
  aes(x = elapsed, y = Size, fill = Method)) +
  geom_density_ridges(aes(y = Method)) +
  facet_wrap(
    ~ Size, 
    labeller = labeller(
      Size = sizes_labs
    )) +
  theme_bw() +
  scale_fill_manual(values = fillcols) +
  theme(axis.text.y = element_blank()) +
  xlab("Elapsed time in seconds") + ylab(NULL) +
  coord_cartesian(xlim = c(0, 75)) +
  theme(text = element_text(family = "AvantGarde")) 

ggsave("sim-figures/time.pdf", width = 8*.8, height = 6*.8)


