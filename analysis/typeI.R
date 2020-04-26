library(ggplot2)
library(ergmito)
library(data.table)
library(xtable)

source("sim-figures/interval_tags.R")
intervals_effect <- c(.1, .5, 1, 2)

# Colors to be used to fill the plots
fillcols <- c(MLE="white", "MC-MLE" = "gray", RM = "black")
fillcols[] <- adjustcolor(fillcols, alpha.f = .7) 

term_name <- c("edges", "ttriad")

# Reading data
res  <- readRDS(sprintf("simulations/%s.rds", "03-various-sizes-4-5-null"))
dgp  <- readRDS(sprintf("simulations/%s-dat.rds", "03-various-sizes-4-5-null"))
pars <- lapply(dgp, "[[", "par")

sizes <- lapply(dgp, "[[", "size")
sizes <- sapply(sizes, sum)
sizes <- sort(unique(unlist(sizes)))

sizes_labs <- structure(paste(sizes, "nets."), names = sizes)

# Alternatively, we are defining the common set to that in which the graphs are
# not fully connected nor are empty
fitted_common <- sapply(dgp, function(d) {
  n <- nvertex(d$nets)
  d <- count_stats(d$nets ~ edges + ttriad)
  any(d[,2] > 0) & any(d[,1] < (n*(n-1)))
})

fitted_common <- which(fitted_common)
nfitted <- length(fitted_common)

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

# Signs of the dgp
signs <- lapply(dgp[fitted_common], "[[", "par")
signs <- do.call(rbind, signs)
signs[] <- sign(signs)

# MC-MLE 
power_mcmle <- lapply(res[fitted_common], "[[", "mcmle") 
power_mcmle <- lapply(power_mcmle, function(i) {
  if (inherits(i, "error"))
    return(matrix(NA, 4, 4))
  i$ci
  })
power_mcmle <- type1_calc(power_mcmle)[,2]


# MLE 
power_mle <- lapply(res[fitted_common], "[[", "mle") 
power_mle <- lapply(power_mle, function(i) {
  if (inherits(i, "error"))
    return(matrix(NA, 4, 4))
  i$ci
})
power_mle <- type1_calc(power_mle)[,2]

# RM
power_rm <- lapply(res[fitted_common], "[[", "rm") 
power_rm <- lapply(power_rm, function(i) {
  if (inherits(i, "error"))
    return(matrix(NA, 4, 4))
  i$ci
})
power_rm <- type1_calc(power_rm)[,2]

# Plot -------------------------------------
dat <- data.frame(
  TypeI = c(as.vector(power_mle), as.vector(power_mcmle), as.vector(power_rm)),
  Method = c(
    rep("MLE", nfitted), rep("MC-MLE", nfitted), rep("RM", nfitted)
  ),
  Term = rep(rep(term_name[2], nfitted), 3),
  Size = sapply(lapply(dgp[fitted_common], "[[", "size") , sum),
  Prop5 = sapply(lapply(dgp[fitted_common], "[[", "size") , function(i) i[2]/sum(i)),
  AvgDensity = sapply(dgp[fitted_common], function(d) {
    mean(nedges(d$nets)/(nvertex(d$nets)*(nvertex(d$nets) - 1)))
  })
  
)
dat <- data.table(dat)

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
  by = .(Method, Term, Size)
  ]
# setnames(dat_tmp, "V1", "Power")

# Making the sample size categorigal
lvls <- sort(unique(dat_tmp$Size))
dat_tmp[, Size := factor(Size, levels = lvls, labels = lvls)]
dat_tmp[, Method := factor(Method, levels = names(fillcols))]

ggplot(dat_tmp, aes(y = TypeI, fill=Method)) +
  geom_col(aes(x = Size, group=Method), position="dodge", color="black") +
  theme_bw() +
  theme(text = element_text(family = "AvantGarde")) +
  scale_fill_manual(values = fillcols) +
  xlab("Sample size") + ylab("Empirical Type I")

# Testing wether the difference is statistically significant. -----

# Aggregating the data
test <- dat_tmp[
  ,
  list(Power = sum(N_TRUE), N = sum(N)),
  by = c("Method", "Size")
  ]

# Preparing the results
Sizes <- sort(unique(test$Size))
differences <- array(
  dim = c(length(Sizes), 8), 
  dimnames = list(Sizes, c("pvalue_rm", "chi2_rm", "pvalue_mcmle", "chi2_mcmle", "N", "MLE", "MC-MLE", "RM"))
)


for (s in Sizes) {
  
  # Making the test
  tmp_test0 <- with(
    test[Size == s & Method != "MC-MLE"],
    prop.test(Power, n = N)
  )
  
  tmp_test1 <- with(
    test[Size == s & Method != "RM"],
    prop.test(Power, n = N)
  )
  
  # Saving the differences
  differences[s, ] <- c(
    tmp_test0$p.value,
    tmp_test0$statistic,
    tmp_test1$p.value,
    tmp_test1$statistic,
    test[Size == s]$N[1],
    tmp_test0$estimate[1],
    tmp_test0$estimate[2],
    tmp_test1$estimate[2]
    )
  
}

differences <- as.data.frame(differences)
differences$Size <- rownames(differences)
rownames(differences) <- seq_len(nrow(differences))

# Taging significant (and formatting)
differences$chi2_rm <- sprintf(
  "%.2f %s", differences$chi2_rm,
  ifelse(differences$pvalue_rm < .01, "***",
         ifelse(differences$pvalue_rm < .05, "**",
                ifelse(differences$pvalue_rm < .10, "*", "")))
)

differences$chi2_mcmle <- sprintf(
  "%.2f %s", differences$chi2_mcmle,
  ifelse(differences$pvalue_mcmle < .01, "***",
         ifelse(differences$pvalue_mcmle < .05, "**",
                ifelse(differences$pvalue_mcmle < .10, "*", "")))
)

differences$N <- formatC(differences$N, big.mark = ",")

# Arranging columns
differences <- differences[c("Size", "N", "MLE", "MC-MLE", "RM", "chi2_mcmle", "chi2_rm")]
colnames(differences)[c(1:2, 6:7)] <- c(
  "Sample size", "N. Simulations", "MC-MLE", "RM"
  )

tab <- xtable(differences, digits=3)
align(tab) <- rep("c", length(align(tab)))
caption(tab) <- paste(
  "\\label{tab:typeI}Empirical Type I error rates.",
  "The $\\chi^2$ statistic is from a 2-sample test for equality of proportions,",
  "and the significance levels are given by *** $p < 0.01$, ** $p < 0.05$, and * $p < 0.10$.",
  "The lack of fitted samples in some levels is due to failure of the estimation method."
)

print(tab, file = "sim-figures/typeI.tex", append = FALSE, booktabs = TRUE,
      include.rownames = FALSE)

# Post processing to group columns
tab <- readLines("sim-figures/typeI.tex")
tab[grepl("\\\\toprule", tab)] <- 
  "\\toprule & & \\multicolumn{3}{c}{P(Type I error)} & \\multicolumn{2}{c}{$\\chi^2$}\\\\ \\cmidrule(r){3-5} \\cmidrule(r){6-7}"
writeLines(tab, "sim-figures/typeI.tex")


