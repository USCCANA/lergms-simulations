library(ggplot2)
library(magrittr)
library(data.table)

source("simulations/interval_tags.R")
intervals_effect <- c(.1, .5, 1, 2)


experiments <- c(
  "Distribution of Empirical Bias (mutual)" = "02-various-sizes-4-5-mutual",
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
  
  sizes_labs <- structure(paste(sizes, "nets."), names = sizes)
  
  # Found by ERGM
  fitted_ergm <- lapply(lapply(res, "[[", "ergm"), "[[", "ci")
  fitted_ergm <- which(
    !sapply(fitted_ergm, is.null) &
      !sapply(fitted_ergm, function(i) any(is.infinite(i))) &
      !sapply(fitted_ergm, function(i) any(is.na(i)))
  )
  
  fitted_ergmito <- lapply(lapply(res, "[[", "ergmito"), "[[", "ci")
  fitted_ergmito <- which(
    !sapply(fitted_ergmito, is.null) &
      !sapply(fitted_ergmito, function(i) any(is.infinite(i))) &
      !sapply(fitted_ergmito, function(i) any(is.na(i)))
  )
  
  fitted_common <- intersect(fitted_ergm, fitted_ergmito)
  nfitted <- length(fitted_common)
  
  # Ergm ---------------------------------------
  power_ergm <- res[fitted_common] %>%
    lapply("[[", "ergm") %>%
    lapply("[[", "ci")
  
  # Edges
  power_ergm_edges <- lapply(power_ergm, "[", i=1, j=) %>%
    do.call(rbind, .) %>%
    {sign(.[,1]) == sign(.[,2])}
  
  # Other term
  power_ergm_term2 <- lapply(power_ergm, "[", i=2, j=) %>%
    do.call(rbind, .) %>%
    {sign(.[,1]) == sign(.[,2])}
  
  # Ergmito ----------------------
  power_ergmito <- res[fitted_common] %>%
    lapply("[[", "ergmito") %>%
    lapply("[[", "ci")
  
  # Edges
  power_ergmito_edges <- lapply(power_ergmito, "[", i=1, j=) %>%
    do.call(rbind, .) %>%
    {sign(.[,1]) == sign(.[,2])}
  
  # Other term
  power_ergmito_term2 <- lapply(power_ergmito, "[", i=2, j=) %>%
    do.call(rbind, .) %>%
    {sign(.[,1]) == sign(.[,2])}
  
  
  # Plot -------------------------------------
  dat <- data.frame(
    Power = c(
      power_ergmito_edges, power_ergmito_term2,
      power_ergm_edges, power_ergm_term2
      ),
    Model = c(
      rep("ergmito", nfitted*2), rep("ergm", nfitted*2)
    ),
    Term = rep(c(rep("edges", nfitted), rep(term_name[2], nfitted)), 2),
    Size = sapply(lapply(dgp[fitted_common], "[[", "size") , sum),
    Par  = c(
      {lapply(dgp[fitted_common], "[[", "par") %>%
        sapply("[", 1)},
      {lapply(dgp[fitted_common], "[[", "par") %>%
        sapply("[", 2)}
    )
      
  ) %>% data.table()
  
  
  
  dat[, EffectSize := interval_tags(abs(Par), intervals_effect)]
  
  dat <- dat[, mean(Power), by = .(Model, Term, Size, EffectSize)]
  setnames(dat, "V1", "Power")
  
  p <- ggplot(dat, aes(y = Power, fill=Model)) +
    geom_col(aes(x = Size, group=Model), position="dodge", color="black") +
    theme_bw() +
    scale_fill_manual(values = fillcols) +
    facet_grid(EffectSize ~ Term) 
  print(p)
  p +
    ggsave(
      sprintf("simulations/power-%s.pdf", e),
      width = 8*.8, height = 6*.8
    )
  
  
}



