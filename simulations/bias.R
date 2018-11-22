library(ggplot2)

experiments <- c(
  "01-fixed-sizes-4",
  "02-various-sizes-3-4"
)

for (e in experiments) {
  
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

  out <- abs(bias$bias) > 10
  
  print(length(which(out)))
  # bias <- bias[which(!out),,drop=FALSE]
  bias[out,1] <- ifelse(bias[out,1] > 0, 10, -10)
  
  g <- ggplot(bias, aes(x = par, y = bias)) +
    geom_jitter() +
    labs(
      title     = "Distribution of Empirical Bias",
      subtitle = "a"
    )
  
  print(g)

  
}

