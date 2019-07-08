library(ggplot2)

experiments <- c(
  "Distribution of Empirical Bias (graphs of size 4)" = "01-fixed-sizes-4",
  "Distribution of Empirical Bias (graphs of sizes 3-5)" = "02-various-sizes-3-5"
)

for (i in seq_along(experiments)) {
  
  e <- experiments[i]
  
  # Reading data
  res <- readRDS(sprintf("simulations/%s.rds", e))
  dgp <- readRDS(sprintf("simulations/%s-dat.rds", e))
  
  nsim <- length(res)
  
  # Computing bias
  bias <- lapply(res, "[[", "coef")
  pars <- lapply(dgp, "[[", "par")
  bias <- do.call(rbind, bias) - do.call(rbind, pars)
  
  bias <- data.frame(
    bias = c(bias[,1], bias[,2]),
    par  = c(rep("edges", nsim), rep("mutual", nsim))
  )

  out <- is.na(bias$bias) 

  print(length(which(out)))
  bias <- bias[which(!out),,drop=FALSE]

  g <- ggplot(bias, aes(x = par, y = bias)) +
    geom_violin() +
    labs(
      # title    = names(experiments)[i],
      # subtitle = sprintf("# of observations %d", sum(!out)/2)
    ) +
    xlab("Parameter") + ylab("Empirical Bias") +
    geom_abline(intercept = -1, slope = 0, lty=2) +
    geom_abline(intercept = 1, slope = 0, lty=2) +
    annotate("text", x = .45, y = 2, label = "1") +
    annotate("text", x = .45, y = -2, label = "-1") +
    theme_light() +
    theme(text = element_text(family = "AvantGarde"))
  
  
  ggsave(
    filename = sprintf("simulations/bias-%s.pdf", e),
    plot = g, width = 8*.8, height = 6*.8
    )

  
}

