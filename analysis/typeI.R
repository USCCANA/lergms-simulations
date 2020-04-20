library(ggplot2)
library(magrittr)
library(ergmito)
library(data.table)
library(xtable)

source("sim-figures/interval_tags.R")
intervals_effect <- c(.1, .5, 1, 2)

# Colors to be used to fill the plots
fillcols <- c(MLE="black", RM="blue", `MC-MLE` = "gray")
fillcols[] <- adjustcolor(fillcols, alpha.f = .7) 

term_name <- c("edges", "ttriad")

# Reading data
res  <- readRDS(sprintf("simulations/%s.rds", "03-various-sizes-4-5-null-rm"))
dgp  <- readRDS(sprintf("simulations/%s-dat.rds", "03-various-sizes-4-5-null"))
pars <- lapply(dgp, "[[", "par")

sizes <- lapply(dgp, "[[", "size")
sizes <- sapply(sizes, sum)
sizes <- sort(unique(unlist(sizes)))

sizes_labs <- structure(paste(sizes, "nets."), names = sizes)

# Checking which methods did not failed
fitted_mcmle <- sapply(res, function(r) {
  if (inherits(r$mcmle, "error"))
    return(FALSE)
  !r$mcmle$degeneracy
})
fitted_mcmle <- which(fitted_mcmle)

fitted_mle <- sapply(res, function(r) {
  if (inherits(r$mle, "error"))
    return(FALSE)
  r$mle$status == 0L
}) 
fitted_mle <- which(fitted_mle)

fitted_rm <- sapply(res, function(r) {
  if (inherits(r$rm, "error"))
    return(FALSE)
  r$rm$degeneracy == 0L
}) 
fitted_rm <- which(fitted_rm)

#' @param dat Should be a vector of the same length as the number of rows in
#' `true_sign`. This has the confidence intervals for each parameters for each
#' model.
#' @param true_sign A matrix with the sign of the true parameters of each model.
type1_calc <- function(dat) {
  
  ans <- matrix(nrow = length(dat), ncol = nrow(dat[[1]]))
  for (k in 1:ncol(ans)) {
    
    tmp <- do.call(rbind, lapply(dat, "[", i = k, j =))
    tmp[] <- sign(tmp)
    
    # All has the same sign
    ans[, k] <- (tmp[,1] == tmp[,2]) 

  }
  
  colnames(ans) <- rownames(dat[[1]])
  ans
  
}

# Compared to MLE vs MC-MLE ----------------------------------------------------

fitted_common <- intersect(fitted_mcmle, fitted_mle)
nfitted <- length(fitted_common)

# Signs of the dgp
signs <- lapply(dgp[fitted_common], "[[", "par") %>%
  do.call(rbind, .)
signs[] <- sign(signs)

# MC-MLE 
power_mcmle <- res[fitted_common] %>%
  lapply("[[", "mcmle") %>%
  lapply("[[", "ci")

power_mcmle <- type1_calc(power_mcmle)[,2]


# MLE 
power_mle <- res[fitted_common] %>%
  lapply("[[", "mle") %>%
  lapply("[[", "ci")

power_mle <- type1_calc(power_mle)[,2]

# RM
power_rm <- res[fitted_common]
power_rm <- lapply(power_rm, "[[", "rm")
power_rm <- lapply(power_rm, "[[", "ci")

power_rm <- type1_calc(power_rm)[,2]

# Plot -------------------------------------
dat <- data.frame(
  TypeI = c(as.vector(power_mle), as.vector(power_mcmle)),
  Model = c(
    rep("MLE", nfitted), rep("MC-MLE", nfitted)
  ),
  Term = rep(rep(term_name[2], nfitted), 2),
  Size = sapply(lapply(dgp[fitted_common], "[[", "size") , sum),
  Prop5 = sapply(lapply(dgp[fitted_common], "[[", "size") , function(i) i[2]/sum(i)),
  AvgDensity = sapply(dgp[fitted_common], function(d) {
    mean(nedges(d$nets)/(nvertex(d$nets)*(nvertex(d$nets) - 1)))
  })
  
) %>% data.table()

dat[, Prop5 := interval_tags(Prop5, c(0, .2, .4, .6, .8, 1))]
dat[, AvgDensity := interval_tags(AvgDensity, c(0, .1, .2, .4, .6, .8, .9, 1))]

# Looking at power by (TERM x SIZE x Effect size) ------------------
dat_tmp <- dat[
  ,
  list(
    TypeI    = mean(TypeI, na.rm = TRUE),
    N_TRUE   = sum(TypeI, na.rm = TRUE),
    N        = .N
  ),
  by = .(Model, Term, Size)
  ]
# setnames(dat_tmp, "V1", "Power")

# Making the sample size categorigal
lvls <- sort(unique(dat_tmp$Size))
dat_tmp[, Size := factor(Size, levels = lvls, labels = lvls)]

ggplot(dat_tmp, aes(y = TypeI, fill=Model)) +
  geom_col(aes(x = Size, group=Model), position="dodge", color="black") +
  theme_bw() +
  theme(text = element_text(family = "AvantGarde")) +
  scale_fill_manual(values = fillcols) +
  xlab("Sample size") + ylab("Empirical Type I")

# Testing wether the difference is statistically significant. -----

# Aggregating the data
test <- dat_tmp[
  ,
  list(Power = sum(N_TRUE), N = sum(N)),
  by = c("Model", "Size")
  ]

# Preparing the results
Sizes <- sort(unique(test$Size))
differences <- array(
  dim = c(length(Sizes), 5), 
  dimnames = list(Sizes, c("p-value", "chi2", "N", "MLE", "MC-MLE"))
)


for (s in Sizes) {
  
  # Making the test
  tmp_test <- with(
    test[Size == s],
    prop.test(Power, n = N)
  )
  
  # Saving the differences
  differences[s, ] <- c(
    tmp_test$p.value,
    tmp_test$statistic,
    test[Size == s]$N[1], tmp_test$estimate)
  
}

differences <- as.data.frame(differences)
differences$Size <- rownames(differences)
rownames(differences) <- seq_len(nrow(differences))

# Taging significant (and formatting)
differences$chi2 <- sprintf(
  "%.2f %s", differences$chi2,
  ifelse(differences$`p-value` < .01, "***",
         ifelse(differences$`p-value` < .05, "**",
                ifelse(differences$`p-value` < .10, "*", "")))
)

differences$N <- formatC(differences$N, big.mark = ",")

# Arranging columns
differences <- differences[c("Size", "N", "MC-MLE", "MLE", "chi2")]
colnames(differences)[1:2] <- c("Sample size", "N. Simulations")

tab <- xtable(differences, digits=3)
align(tab) <- rep("c", length(align(tab)))
caption(tab) <- paste(
  "\\label{tab:typeI}Empirical Type I error rates.",
  "The $\\chi^2$ statistic is from a 2-sample test for equality of proportions,",
  "and the significance levels are given by *** $p < 0.01$, ** $p < 0.05$, and * $p < 0.10$.",
  "The lack of fitted samples in some levels is due to failure of the estimation method."
)

print(tab, file = "sim-figures/typeI-mle-mcmle.tex", append = FALSE, booktabs = TRUE,
      include.rownames = FALSE)

# Post processing to group columns
tab <- readLines("sim-figures/typeI-mle-mcmle.tex")
tab[grepl("\\\\toprule", tab)] <- 
  "\\toprule & & \\multicolumn{2}{c}{P(Type I error)} \\\\ \\cmidrule(r){3-4}"
writeLines(tab, "sim-figures/typeI-mle-mcmle.tex")

# This returns an error iff differences are significant!
# stopifnot(all(differences > .1))

# Compared to MLE vs MC-MLE vs RM ----------------------------------------------

fitted_common <- intersect(fitted_rm, fitted_mle)
nfitted <- length(fitted_common)

# Signs of the dgp
signs <- lapply(dgp[fitted_common], "[[", "par") %>%
  do.call(rbind, .)
signs[] <- sign(signs)

# MLE 
power_mle <- res[fitted_common] %>%
  lapply("[[", "mle") %>%
  lapply("[[", "ci")

power_mle <- type1_calc(power_mle)[,2]

# RM
power_rm <- res[fitted_common]
power_rm <- lapply(power_rm, "[[", "rm")
power_rm <- lapply(power_rm, "[[", "ci")

power_rm <- type1_calc(power_rm)[,2]

# Plot -------------------------------------
dat <- data.frame(
  TypeI = c(as.vector(power_mle), as.vector(power_rm)),
  Model = c(
    rep("MLE", nfitted), rep("RM", nfitted)
  ),
  Term = rep(rep(term_name[2], nfitted), 2),
  Size = sapply(lapply(dgp[fitted_common], "[[", "size") , sum),
  Prop5 = sapply(lapply(dgp[fitted_common], "[[", "size") , function(i) i[2]/sum(i)),
  AvgDensity = sapply(dgp[fitted_common], function(d) {
    mean(nedges(d$nets)/(nvertex(d$nets)*(nvertex(d$nets) - 1)))
    })
    
) %>% data.table()

dat[, Prop5 := interval_tags(Prop5, c(0, .2, .4, .6, .8, 1))]
dat[, AvgDensity := interval_tags(AvgDensity, c(0, .1, .2, .4, .6, .8, .9, 1))]

# Looking at power by (TERM x SIZE x Effect size) ------------------
dat_tmp <- dat[
  ,
  list(
    TypeI    = mean(TypeI, na.rm = TRUE),
    N_TRUE   = sum(TypeI, na.rm = TRUE),
    N        = .N
  ),
  by = .(Model, Term, Size)
  ]
# setnames(dat_tmp, "V1", "Power")

# Making the sample size categorigal
lvls <- sort(unique(dat_tmp$Size))
dat_tmp[, Size := factor(Size, levels = lvls, labels = lvls)]

ggplot(dat_tmp, aes(y = TypeI, fill=Model)) +
  geom_col(aes(x = Size, group=Model), position="dodge", color="black") +
  theme_bw() +
  theme(text = element_text(family = "AvantGarde")) +
  scale_fill_manual(values = fillcols) +
  xlab("Sample size") + ylab("Empirical Type I")

# Testing wether the difference is statistically significant. -----

# Aggregating the data
test <- dat_tmp[
  ,
  list(Power = sum(N_TRUE), N = sum(N)),
  by = c("Model", "Size")
  ]

# Preparing the results
Sizes <- sort(unique(test$Size))
differences <- array(
  dim = c(length(Sizes), 5), 
  dimnames = list(Sizes, c("p-value", "chi2", "N", "MLE", "RM"))
)


# Vs RM
for (s in Sizes) {
  
  # Making the test
  tmp_test <- with(
    test[Size == s & Model %in% c("MLE", "RM")],
    prop.test(Power, n = N)
  )
  
  # Saving the differences
  differences[s, ] <- c(
    tmp_test$p.value,
    tmp_test$statistic,
    test[Size == s]$N[1], tmp_test$estimate)
  
}

differences <- as.data.frame(differences)
differences$Size <- rownames(differences)
rownames(differences) <- seq_len(nrow(differences))

# Taging significant (and formatting)
differences$chi2 <- sprintf(
  "%.2f %s", differences$chi2,
  ifelse(differences$`p-value` < .01, "***",
         ifelse(differences$`p-value` < .05, "**",
                ifelse(differences$`p-value` < .10, "*", "")))
  )

differences$N <- formatC(differences$N, big.mark = ",")

# Arranging columns
differences <- differences[c("Size", "N", "RM", "MLE", "chi2")]
colnames(differences)[1:2] <- c("Sample size", "N. Simulations")

tab <- xtable(differences, digits=3)
align(tab) <- rep("c", length(align(tab)))
caption(tab) <- paste(
  "\\label{tab:typeI}Empirical Type I error rates.",
  "The $\\chi^2$ statistic is from a 2-sample test for equality of proportions,",
  "and the significance levels are given by *** $p < 0.01$, ** $p < 0.05$, and * $p < 0.10$.",
  "The lack of fitted samples in some levels is due to failure of the estimation method."
)
print(tab, file = "sim-figures/typeI-rm-mle.tex", append = FALSE, booktabs = TRUE,
      include.rownames = FALSE)

# Post processing to group columns
tab <- readLines("sim-figures/typeI-rm-mle.tex")
tab[grepl("\\\\toprule", tab)] <- 
  "\\toprule & & \\multicolumn{2}{c}{P(Type I error)} \\\\ \\cmidrule(r){3-4}"
writeLines(tab, "sim-figures/typeI-rm-mle.tex")

# This returns an error iff differences are significant!
# stopifnot(all(differences > .1))

