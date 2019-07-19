library(magrittr)
library(ggplot2)
library(ggforce)
library(data.table)

experiments <- c(
  "Distribution of Empirical Bias: ttriad" = "02-various-sizes-4-5-ttriad" # ,
  # "Distribution of Empirical Bias: mutual" = "02-various-sizes-4-5-mutual"
)

for (i in seq_along(experiments)) {
  
  e <- experiments[i]
  
  # Reading data
  res <- readRDS(sprintf("simulations/%s.rds", e))
  dgp <- readRDS(sprintf("simulations/%s-dat.rds", e))
  
  # Looking at the common set of estimations
  ok_ergm <- lapply(res, "[[", "ergm")
  ok_ergm <- which(!sapply(ok_ergm, inherits, what = "error"))
  
  # finite
  finite_ergm <- lapply(res[ok_ergm], "[[", "ergm") %>%
    sapply("[[", "coef") %>%
    t
  finite_ergm[!is.finite(finite_ergm)] <- NA
  ok_ergm <- ok_ergm[which(complete.cases(finite_ergm))]
  
  ok_ergmito <- lapply(res, "[[", "ergmito")
  ok_ergmito <- which(!sapply(ok_ergmito, inherits, what = "error"))
  
  # finite
  finite_ergmito <- lapply(res[ok_ergmito], "[[", "ergmito") %>%
    sapply("[[", "coef") %>%
    t
  finite_ergmito[!is.finite(finite_ergmito)] <- NA
  ok_ergmito <- ok_ergmito[which(complete.cases(finite_ergmito))]
  
  ok <- intersect(ok_ergm, ok_ergmito)
  
  # Computing bias
  pars <- do.call(rbind, lapply(dgp[ok], "[[", "par"))
  
  bias_ergm <- lapply(res[ok], "[[", "ergm")
  bias_ergm <- do.call(rbind, lapply(bias_ergm, "[[", "coef")) -  pars
  
  bias_ergmito <- lapply(res[ok], "[[", "ergmito")
  bias_ergmito <- do.call(rbind, lapply(bias_ergmito, "[[", "coef")) -  pars
  
  # Relative bias distribution
  bias_edges <- abs(bias_ergm[,1])/abs(bias_ergmito[,1])
  bias_term2 <- abs(bias_ergm[,2])/abs(bias_ergmito[,2])
  

  S <- c(.01, .025, .1, .25, .5, .75, .9, .975, .99)
  barplot(quantile(bias_edges, S, na.rm = TRUE));abline(a=1, b=0)
  barplot(quantile(bias_term2, S, na.rm = TRUE));abline(a=1, b=0)
  
  
  dat <- data.frame(
    BiasDif = c(bias_edges, bias_term2),
    Term    = c(
      rep("edges", length(bias_edges)),
      rep(gsub(".+[:]\\s+", "", names(e)), length(bias_term2))
      )
  ) %>% data.table()
  
  p <- ggplot(dat, aes(y = BiasDif, x = Term)) +
    geom_violin() +
    ylab("|ergm Bias|/|ergmito Bias|") +
    theme(text = element_text(family = "AvantGarde")) +
    facet_zoom(y = BiasDif < 10) +
    geom_abline(intercept = 1, slope=0, lty=2) 
  
  p + ggsave(sprintf("simulations/bias-relative-%s.pdf", e),
            width = 8*.8, height = 6*.8)
  
  print(p)
  
}
