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
fillcols <- c(mle="black", rm="blue", mcmle = "gray")
fillcols[] <- adjustcolor(fillcols, alpha.f = .7) 

for (i in seq_along(experiments)) {
  
  e         <- experiments[i]
  term_name <- c("edges", gsub(".+[-](?=[a-zA-Z]+$)", "", e, perl = TRUE))
  
  # Reading data
  res  <- readRDS("simulations/02-various-sizes-4-5-ttriad-rm.rds") # readRDS(sprintf("simulations/%s.rds", e))
  dgp  <- readRDS(sprintf("simulations/%s-dat.rds", e))
  pars <- lapply(dgp, "[[", "par")
  
  sizes <- lapply(dgp, "[[", "size")
  sizes <- sapply(sizes, sum)
  sizes <- sort(unique(unlist(sizes)))
  
  sizes_labs <- structure(paste(sizes, "nets"), names = sizes)
  
  # Found by ERGM
  fitted_mcmle <- sapply(res, function(r) {
    if (inherits(r$mcmle, "error"))
      return(FALSE)
    !r$mcmle$degeneracy
  }) %>% which
  
  fitted_mle <- sapply(res, function(r) {
    if (inherits(r$mle, "error"))
      return(FALSE)
    r$mle$status == 0L
  }) %>% which
  
  fitted_rm <- sapply(res, function(r) {
    if (inherits(r$rm, "error"))
      return(FALSE)
    r$rm$degeneracy == 0L
  }) %>% which
  
  fitted_common <- intersect(intersect(fitted_mcmle, fitted_mle), fitted_rm)
  nfitted <- length(fitted_common)
  
  pars <- lapply(dgp, "[[", "par")

  bias_mle <- lapply(lapply(res, "[[", "mle"), "[[", "coef")[fitted_common]
  bias_mle <- do.call(rbind, bias_mle) - do.call(rbind, pars[fitted_common])
  
  bias_mcmle <- lapply(lapply(res, "[[", "mcmle"), "[[", "coef")[fitted_common]
  bias_mcmle <- do.call(rbind, bias_mcmle) - do.call(rbind, pars[fitted_common])
  
  bias_rm <- lapply(lapply(res, "[[", "rm"), "[[", "coef")[fitted_common]
  bias_rm <- do.call(rbind, bias_rm) - do.call(rbind, pars[fitted_common])

  
  # Side by side bias
  dat <- data.frame(
    abs_bias = c(bias_mcmle[,1], bias_mle[,1], bias_rm[,1], bias_mcmle[,2], bias_mle[,2], bias_rm[,2]),
    term     = c(rep(term_name[1], nfitted*3), rep(term_name[2], nfitted*3)),
    model    = rep(c(rep("mcmle", nfitted), rep("mle", nfitted), rep("rm", nfitted)), 2),
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
    labs(fill = "Method") +
    ylab(NULL) +
    theme(
      axis.ticks.x  = element_blank(),
      axis.text.x  = element_blank(),
      text = element_text(family = "AvantGarde")
      ) 
  print(p)
  # p
  # +
  #   ggsave(
  #     sprintf("simulations/bias-%s.pdf", e),
  #     width = 8*.8, height = 6*.8
  #     )
  #

  # Time -----------------------------------------------------------------------
  
  times_mcmle <- lapply(res[fitted_common], "[[", "mcmle") %>%
    lapply("[[", "time") %>%
    do.call(rbind, .) %>%
    data.table() %>%
    .[, Model := "mcmle"]
    
  times_mle <- lapply(res[fitted_common], "[[", "mle") %>%
    lapply("[[", "time") %>%
    do.call(rbind, .) %>%
    data.table() %>%
    .[, Model := "mle"]
  
  times_rm <- lapply(res[fitted_common], "[[", "rm") %>%
    lapply("[[", "time") %>%
    do.call(rbind, .) %>%
    data.table() %>%
    .[, Model := "rm"]
  
  times_mcmle[, Size        := sapply(lapply(dgp[fitted_common], "[[", "size"), sum)]
  times_mcmle[, Relative    := elapsed/times_mle$elapsed]
  times_rm[, Size        := sapply(lapply(dgp[fitted_common], "[[", "size"), sum)]
  times_rm[, Relative    := elapsed/times_mle$elapsed]
  times_mle[, Size     := sapply(lapply(dgp[fitted_common], "[[", "size"), sum)]
  times_mle[, Relative := 1.0]
  
  p <- rbindlist(list(times_mle, times_rm, times_mcmle)) %>%
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
  
  p 
  # +
  #   ggsave(sprintf("simulations/bias-elapsed-%s.pdf", e),
  #          width = 8*.8, height = 6*.8)
  
  # ggplot(times_ergm, aes(y = Relative, x = "")) +
  #   geom_violin() + scale_y_log10()
  # 
  
  
  # ERGM is usually biased  
  # ggplot(dat2, aes(y=abs_bias, x=term)) + 
  #   geom_violin(aes(group=term)) #+ scale_y_log10()
}

