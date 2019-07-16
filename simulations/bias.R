library(ggplot2)

source("simulations/interval_tags.R")

experiments <- c(
  "Distribution of Empirical Bias (ttriad)" = "02-various-sizes-4-5-ttriad"
  # "Distribution of Empirical Bias (mutual)" = "02-various-sizes-4-5-mutual"
)

for (i in seq_along(experiments)) {
  
  e <- experiments[i]
  
  # Reading data
  res <- readRDS(sprintf("simulations/%s.rds", e))
  dgp <- readRDS(sprintf("simulations/%s-dat.rds", e))
  
  for (m in c("ergm", "ergmito")) {
  
    nsim <- length(res) 
    
    # Computing bias
    bias <- lapply(lapply(res, "[[", m), "[[", "coef")
    
    idx_fitted <- which(!sapply(bias, is.null))
    
    pars <- lapply(dgp, "[[", "par")
    bias <- do.call(rbind, bias[idx_fitted]) - do.call(rbind, pars[idx_fitted])
    rownames(bias) <- idx_fitted

    counts <- sapply(lapply(dgp[idx_fitted], "[[", "size"), sum)
    
    bias <- data.frame(
      bias = c(bias[,1], bias[,2]),
      par  = c(rep("edges", nrow(bias)), rep("mutual", nrow(bias))),
      n    = c(counts, counts)
    )
    
    # # Building intervals
    # bias$n3_tags <- interval_tags(
    #   bias$n3, quantile(bias$n3, c(0, .25, .5, .75, 1))
    #   )
  
    g <- ggplot(bias, aes(x = n, y = bias)) +
      geom_violin(aes(group = n)) +
      ylim(-10, 10) +
      facet_grid(cols = vars(par))  +
      labs(
        title    = paste(names(experiments)[i], "fitted using", m),
        subtitle = sprintf(
          "# of observations %d (%d discarded due to Inf)", nrow(bias)/2,
          nsim - nrow(bias)/2
          )
      ) +
      xlab("Sample size") + ylab("Empirical Bias") +
      geom_abline(intercept = -1, slope = 0, lty=2) +
      geom_abline(intercept = 1, slope = 0, lty=2) +
      annotate("text", x = .45, y = 2, label = "1") +
      annotate("text", x = .45, y = -2, label = "-1") +
      theme_light() +
      theme(text = element_text(family = "AvantGarde"))
    
    print(g)

    ggsave(
      filename = sprintf("simulations/bias-%s-%s.pdf", e, m),
      plot = g, width = 8*.8, height = 6*.8
      )

  }
  
  # Side by side coverage ------------------------------------------------------
  
  # Looking at a common set of observations
  
  # Found by ERGM
  fitted_ergm <- lapply(lapply(res, "[[", "ergm"), "[[", "coef")
  fitted_ergm <- which(!sapply(fitted_ergm, is.null))
  
  fitted_ergmito <- lapply(lapply(res, "[[", "ergmito"), "[[", "coef")
  fitted_ergmito <- which(!sapply(fitted_ergmito, is.null))
  
  fitted_common <- intersect(fitted_ergm, fitted_ergmito)
  
  pars <- lapply(dgp, "[[", "par")

  bias_ergm <- lapply(lapply(res, "[[", "ergm"), "[[", "coef")[fitted_common]
  bias_ergm <- do.call(rbind, bias_ergm) - do.call(rbind, pars[fitted_common])
  
  bias_ergmito <- lapply(lapply(res, "[[", "ergmito"), "[[", "coef")[fitted_common]
  bias_ergmito <- do.call(rbind, bias_ergmito) - do.call(rbind, pars[fitted_common])

  nfitted <- length(fitted_common)
  
  dat <- data.frame(
    abs_bias = c(bias_ergm[,1], bias_ergmito[,1], bias_ergm[,2], bias_ergmito[,2]),
    term     = c(rep("edges", nfitted*2), rep("ttriad", nfitted*2)),
    model    = rep(c(rep("ergm", nfitted), rep("ergmito", nfitted)), 2)
  )
  
  ggplot(dat, aes(y=abs_bias, x=term)) + 
    geom_violin(aes(group=term)) + facet_grid(rows = vars(model))
  
  dat2 <- data.frame(
    abs_bias = c(
      abs(bias_ergm[,1])- abs(bias_ergmito[,1]), abs(bias_ergm[,2]) - abs(bias_ergmito[,2])),
    term     = c(rep("edges", nfitted), rep("ttriad", nfitted)),
    model    = c(rep("ergm", nfitted), rep("ergmito", nfitted))
  )

  # ERGM is usually biased  
  ggplot(dat2, aes(y=abs_bias, x=term)) + 
    geom_violin(aes(group=term)) + scale_y_log10()
}

