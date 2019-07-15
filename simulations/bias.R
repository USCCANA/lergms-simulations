library(ggplot2)

source("simulations/interval_tags.R")

experiments <- c(
  # "Distribution of Empirical Bias (graphs of size 4)" = "01-fixed-sizes-4",
  "Distribution of Empirical Bias (graphs of sizes 3-5)" = "02-various-sizes-3-5"
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
    pars <- lapply(dgp, "[[", "par")
    bias <- do.call(rbind, bias) - do.call(rbind, pars)
    
    # Droping the Inf
    bias[!is.finite(bias)] <- NA
    out <- complete.cases(bias)
    
    counts <- sapply(lapply(dgp, "[[", "size"), sum)
    
    bias <- data.frame(
      bias = c(bias[,1], bias[,2]),
      par  = c(rep("edges", nsim), rep("mutual", nsim)),
      n3   = c(counts, counts)
    )[out,]
    
    # Building intervals
    bias$n3_tags <- interval_tags(
      bias$n3, quantile(bias$n3, c(0, .25, .5, .75, 1))
      )
  
    g <- ggplot(bias, aes(x = n3_tags, y = bias)) +
      geom_violin(aes(group = n3_tags)) +
      facet_grid(cols = vars(par))  +
      labs(
        title    = paste(names(experiments)[i], "fitted using", m),
        subtitle = sprintf("# of observations %d (%d discarded due to Inf)", sum(out), sum(!out))
      ) +
      xlab("Sample size") + ylab("Empirical Bias") +
      geom_abline(intercept = -1, slope = 0, lty=2) +
      geom_abline(intercept = 1, slope = 0, lty=2) +
      annotate("text", x = .45, y = 2, label = "1") +
      annotate("text", x = .45, y = -2, label = "-1") +
      theme_light() +
      theme(text = element_text(family = "AvantGarde"))
    
    print(g)
    # stop()
    ggsave(
      filename = sprintf("simulations/bias-%s-%s.pdf", e, m),
      plot = g, width = 8*.8, height = 6*.8
      )
    
    

  }
}

