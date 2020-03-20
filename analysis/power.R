library(ggplot2)
library(magrittr)
library(ergmito)
library(data.table)

source("simulations/interval_tags.R")
intervals_effect <- c(.1, .5, 1, 2)

experiments <- c(
  # "Distribution of Empirical Bias (mutual)" = "02-various-sizes-4-5-mutual",
  "Distribution of Empirical Bias (ttriad)" = "02-various-sizes-4-5-ttriad"
)

# Colors to be used to fill the plots
fillcols <- c(ergm = "gray", ergmito="black")
fillcols[] <- adjustcolor(fillcols, alpha.f = .7) 

e         <- experiments[1]
term_name <- c("edges", gsub(".+[-](?=[a-zA-Z]+$)", "", e, perl = TRUE))

# Reading data
res  <- readRDS(sprintf("simulations/%s.rds", e))
res2 <- readRDS("02-various-sizes-4-5-ttriad.rds")
dgp  <- readRDS(sprintf("simulations/%s-dat.rds", e))
pars <- lapply(dgp, "[[", "par")

sizes <- lapply(dgp, "[[", "size")
sizes <- sapply(sizes, sum)
sizes <- sort(unique(unlist(sizes)))

sizes_labs <- structure(paste(sizes, "nets."), names = sizes)

# Found by ERGM
fitted_mcmle <- sapply(res, function(r) {
  if (inherits(r$ergm, "error"))
    return(FALSE)
  !r$ergm$degeneracy
}) %>% which

fitted_mle <- sapply(res, function(r) {
  if (inherits(r$ergmito, "error"))
    return(FALSE)
  r$ergmito$status == 0L
}) %>% which


fitted_common <- intersect(fitted_mcmle, fitted_mle)
nfitted <- length(fitted_common)

# Signs of the dgp
signs <- lapply(dgp[fitted_common], "[[", "par") %>%
  do.call(rbind, .)
signs[] <- sign(signs)

#' @param dat Should be a vector of the same length as the number of rows in
#' `true_sign`. This has the confidence intervals for each parameters for each
#' model.
#' @param true_sign A matrix with the sign of the true parameters of each model.
power_calc <- function(dat, true_sign) {
  
  ans <- matrix(nrow = length(dat), ncol = ncol(true_sign))
  for (k in 1:ncol(ans)) {
    
    tmp <- do.call(rbind, lapply(dat, "[", i = k, j =))
    tmp[] <- sign(tmp)
    
    # All has the same sign
    ans[,k] <- (tmp[,1] == tmp[,2]) & (tmp[,1] == true_sign[, k])
    
  }
  
  colnames(ans) <- rownames(dat[[1]])
  ans
  
}

# Ergm ---------------------------------------

power_mcmle <- res[fitted_common] %>%
  lapply("[[", "ergm") %>%
  lapply("[[", "ci")

power_mcmle <- power_calc(power_mcmle, signs)


# Ergmito ----------------------
power_mle <- res[fitted_common] %>%
  lapply("[[", "ergmito") %>%
  lapply("[[", "ci")

power_mle2 <- res2[fitted_common] %>%
  lapply("[[", "ergmito") %>%
  lapply("[[", "ci")

# Comparing vcovs
difference <- parallel::mcMap(function(x1, x2) sqrt(sum((x1 - x2)^2)), 
                x1 = power_mle, x2 = power_mle2)
difference <- unlist(difference)
difference <- which(difference > 1)
power_mle2[difference]
power_mle[difference]

power_mle <- power_calc(power_mle, signs)
power_mle2 <- power_calc(power_mle2, signs)

# Plot -------------------------------------
dat <- data.frame(
  Power = c(as.vector(power_mle2), as.vector(power_mcmle)),
  Model = c(
    rep("ergmito", nfitted*2), rep("ergm", nfitted*2)
  ),
  Term = rep(c(rep("edges", nfitted), rep(term_name[2], nfitted)), 2),
  Size = sapply(lapply(dgp[fitted_common], "[[", "size") , sum),
  Prop5 = sapply(lapply(dgp[fitted_common], "[[", "size") , function(i) i[2]/sum(i)),
  Par  = c(
    {lapply(dgp[fitted_common], "[[", "par") %>%
      sapply("[", 1)},
    {lapply(dgp[fitted_common], "[[", "par") %>%
      sapply("[", 2)}
  ),
  AvgDensity = sapply(dgp[fitted_common], function(d) {
    mean(nedges(d$nets)/(nvertex(d$nets)*(nvertex(d$nets) - 1)))
    })
    
) %>% data.table()



dat[, EffectSize := interval_tags(abs(Par), intervals_effect)]
dat[, Prop5 := interval_tags(Prop5, c(0, .2, .4, .6, .8, 1))]
dat[, AvgDensity := interval_tags(AvgDensity, c(0, .1, .2, .4, .6, .8, .9, 1))]

# Looking at power by (TERM x SIZE x Effect size) ------------------
dat_tmp <- dat[
  ,
  list(
    Power    = mean(Power, na.rm = TRUE),
    N_TRUE   = sum(Power, na.rm = TRUE),
    N        = .N
  ),
  by = .(Model, Term, Size, EffectSize)
  ]
# setnames(dat_tmp, "V1", "Power")

# Making the sample size categorigal
lvls <- sort(unique(dat_tmp$Size))
dat_tmp[, Size := factor(Size, levels = lvls, labels = lvls)]

p <- ggplot(dat_tmp, aes(y = Power, fill=Model)) +
  geom_col(aes(x = Size, group=Model), position="dodge", color="black") +
  theme_bw() +
  theme(text = element_text(family = "AvantGarde")) +
  scale_fill_manual(values = fillcols) +
  facet_grid(EffectSize ~ Term) +
  xlab("Sample size") + ylab("Empirical power")

print(p)

ggsave(
    sprintf("analysis/power-by-model.pdf", e),
    width = 8*.8, height = 6*.8
  )
  
# Testing wether the difference is statistically significant. ------------------

# Aggregating the data
test <- dat_tmp[
  ,
  list(Power = sum(N_TRUE), N = sum(N)),
  by = c("Model", "Term", "Size", "EffectSize")
  ]

# Preparing the results
Sizes <- sort(unique(test$Size))
Esizes <- sort(unique(test$EffectSize))
differences <- array(
  dim = c(length(Sizes), 2, length(Esizes)), 
  dimnames = list(Sizes, c("edges", "ttriad"), Esizes)
)

for (e in Esizes) {
  for (term in c("edges", "ttriad")) {
    for (s in Sizes) {
      
      # Making the test
      tmp_test <- with(
        test[Term == term & Size == s & EffectSize == e],
        prop.test(Power, n = N)
      )
      
      # Saving the differences
      differences[s, term, e] <- tmp_test$p.value
      
    }
  }
}

# This returns an error iff differences are significant!
stopifnot(all(differences > .1))




# Power by average density -----------------------------------------------------
dat_tmp <- dat[
  , 
  list(
    Power = mean(Power, na.rm=TRUE)
    ),
  by = .(Model, Term, AvgDensity, EffectSize)
  ]

# Making the sample size categorigal
lvls <- sort(unique(dat_tmp$AvgDensity))
dat_tmp[, AvgDensity := factor(AvgDensity, levels = lvls, labels = lvls)]

ggplot(dat_tmp, aes(y = Power, fill=Model)) +
  geom_col(aes(x = AvgDensity, group=Model), position="dodge", color="black") +
  theme_bw() +
  theme(text = element_text(family = "AvantGarde")) +
  scale_fill_manual(values = fillcols) +
  facet_grid(EffectSize ~ Term) +
  xlab("Average Density") + ylab("Empirical power")


# Looking at power by (TERM x SIZE x Effect size) ------------------------------
dat_tmp <- dat[, list(Power = mean(Power, na.rm=TRUE)), by = .(Model, Term, Size, Prop5)]

# Making the sample size categorigal
lvls <- c(5, 30, 50, 100, 300)
dat_tmp[, Size := interval_tags(Size, lvls)]

p <- ggplot(dat_tmp[Model == "ergmito"], aes(y = Power, fill = Prop5)) +
  geom_col(aes(x = Prop5), position="dodge") +
  theme_bw() +
  theme(text = element_text(family = "AvantGarde")) +
  scale_fill_viridis_d() +
  facet_grid(Size ~ Term) +
  xlab("") + ylab("Empirical power") +
  labs(fill = "Prop. of nets.\nof size 5") +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
    )

p +
  ggsave(
    sprintf("analysis/power-by-prop-of-fives.pdf", e),
    width = 8*.8, height = 6*.8
  )


# Analyzing cases in which MC-MLE did better than MLE --------------------------

# MC-MLE identified both, MLE did not or vice versa
both_mcmle <- which(
  (power_mcmle[,1] & power_mcmle[,2]) &
    (!power_mle[,1] & !power_mle[,2])
  )

both_mcmle # three cases in which MCMLE discovered something on both, but MLE did
# not

lapply(res[fitted_common][both_mcmle], "[[", "ergmito") %>%
  lapply("[[", "coef") %>% do.call(rbind, .)

lapply(res[fitted_common][both_mcmle], "[[", "ergm") %>%
  lapply("[[", "coef") %>% do.call(rbind, .)

library(ergmito)

net. <- dgp[fitted_common][both_mcmle][[1]]$nets
ans. <- ergmito(net. ~ edges + ctriad, ntries = 10, init = res[fitted_common][both_mcmle][[1]]$ergmito$coef)

both_mle <- which(
  (!power_mcmle[,1] & !power_mcmle[,2]) &
    (power_mle[,1] & power_mle[,2])
)

both_mle
