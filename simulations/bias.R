library(ggplot2)
library(ggforce)
library(ggridges)
library(magrittr)
library(data.table)

source("simulations/interval_tags.R")

experiments <- c(
  # "Distribution of Empirical Bias (mutual)" = "02-various-sizes-4-5-mutual",
  "Distribution of Empirical Bias (ttriad)" = "02-various-sizes-4-5-ttriad"
)

# Colors to be used to fill the plots
fillcols <- c(ergm = "gray", ergmito="black")
fillcols[] <- adjustcolor(fillcols, alpha.f = .7) 

for (i in seq_along(experiments)) {
  
  e         <- experiments[i]
  term_name <- c("edges", gsub(".+[-](?=[a-zA-Z]+$)", "", e, perl = TRUE))
  
  # Reading data
  res  <- readRDS(sprintf("simulations/%s.rds", e))
  dgp  <- readRDS(sprintf("simulations/%s-dat.rds", e))
  pars <- lapply(dgp, "[[", "par")
  
  sizes <- lapply(dgp, "[[", "size")
  sizes <- sapply(sizes, sum)
  sizes <- sort(unique(unlist(sizes)))
  
  sizes_labs <- structure(paste(sizes, "nets"), names = sizes)
  
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
  
  pars <- lapply(dgp, "[[", "par")

  bias_ergm <- lapply(lapply(res, "[[", "ergm"), "[[", "coef")[fitted_common]
  bias_ergm <- do.call(rbind, bias_ergm) - do.call(rbind, pars[fitted_common])
  
  bias_ergmito <- lapply(lapply(res, "[[", "ergmito"), "[[", "coef")[fitted_common]
  bias_ergmito <- do.call(rbind, bias_ergmito) - do.call(rbind, pars[fitted_common])

  
  # Side by side bias
  dat <- data.frame(
    abs_bias = c(bias_ergm[,1], bias_ergmito[,1], bias_ergm[,2], bias_ergmito[,2]),
    term     = c(rep(term_name[1], nfitted*2), rep(term_name[2], nfitted*2)),
    model    = rep(c(rep("ergm", nfitted), rep("ergmito", nfitted)), 2),
    size     = sapply(lapply(dgp[fitted_common], "[[", "size"), sum),
    stringsAsFactors = FALSE
  )
  
  p <- dat %>% 
  ggplot(aes(abs_bias, y=model)) + 
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
    labs(fill = "Model") +
    ylab(NULL) +
    theme(
      axis.ticks.x  = element_blank(),
      axis.text.x  = element_blank(),
      text = element_text(family = "AvantGarde")
      ) 
  print(p)
  p +
    ggsave(
      sprintf("simulations/bias-%s.pdf", e),
      width = 8*.8, height = 6*.8
      )
  
  # Absolute bias size
  dat <- data.frame(
    Diff = c(abs(bias_ergm[,1]) - abs(bias_ergmito[,1]), abs(bias_ergm[,2]) - abs(bias_ergmito[,2])),
    Term = c(rep("edges", nfitted), rep(term_name[2], nfitted)),
    size = rep(sapply(lapply(dgp[fitted_common], "[[", "size"), sum), 2)
  ) 
  
  dat_test <- dat %>%
    data.table() %>%
    .[, t.test(Diff)[c("estimate", "p.value")], by=Term]
  
  p <- ggplot(dat, aes(y=Diff)) +
    geom_jitter(aes(x=Term), color = adjustcolor("black", alpha.f = .5)) +
    facet_zoom(y = Diff > -3 & Diff < 3) +
    ylab("|ergm bias| - |ergmito bias|") +
    xlab("Term") +
    theme(text = element_text(family = "AvantGarde"))
  
  print(p)
  
  p + ggsave(sprintf("simulations/bias-absdiff-%s.pdf", e),
           width = 8*.8, height = 6*.8)
  
  
  
  
    
  # Time -----------------------------------------------------------------------
  
  times_ergm <- lapply(res[fitted_common], "[[", "ergm") %>%
    lapply("[[", "time") %>%
    do.call(rbind, .) %>%
    data.table() %>%
    .[, Model := "ergm"]
    
  times_ergmito <- lapply(res[fitted_common], "[[", "ergmito") %>%
    lapply("[[", "time") %>%
    do.call(rbind, .) %>%
    data.table() %>%
    .[, Model := "ergmito"]
  
  times_ergm[, Size        := sapply(lapply(dgp[fitted_common], "[[", "size"), sum)]
  times_ergm[, Relative    := elapsed/times_ergmito$elapsed]
  times_ergmito[, Size     := sapply(lapply(dgp[fitted_common], "[[", "size"), sum)]
  times_ergmito[, Relative := 1.0]
  
  p <- rbindlist(list(times_ergm, times_ergmito)) %>%
    ggplot(aes(x = elapsed, y = Size, fill = Model)) +
    geom_density_ridges(aes(y = Model)) +
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
  
  print(p)
  
  p +
    ggsave(sprintf("simulations/bias-elapsed-%s.pdf", e),
           width = 8*.8, height = 6*.8)
  
  # ggplot(times_ergm, aes(y = Relative, x = "")) +
  #   geom_violin() + scale_y_log10()
  # 
  
  
  # ERGM is usually biased  
  # ggplot(dat2, aes(y=abs_bias, x=term)) + 
  #   geom_violin(aes(group=term)) #+ scale_y_log10()
}

