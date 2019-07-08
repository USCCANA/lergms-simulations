library(ggplot2)
library(magrittr)

source("simulations/interval_tags.R")
intervals        <- c(1, 20, 30, 40, 50, 100)
intervals_effect <- c(.2, .5, 1, 2, 2.5)

experiments <- c(
   "Distributio of Power by Sample size (Networks of size 4)" = "01-fixed-sizes-4",
   "Distributio of Power by Sample size (Networks of size 3 and 4)" = "02-various-sizes-3-5" #,
   # "Distributio of Power by Sample size (Networks of size 4, bootstrap CI)" = "01-fixed-sizes-4-boot",
   # "Distributio of Power by Sample size (Networks of size 3 and 4, bootstrap CI)" = "02-various-sizes-3-4-boot"
)


for (i in seq_along(experiments)) {
  
  e <- experiments[i]
  
  # Reading data
  res <- readRDS(sprintf("simulations/%s.rds", e))
  dgp <- readRDS(sprintf("simulations/%s-dat.rds", gsub("-boot$", "", e)))
  
  
  nsim <- length(res)
  
  # Complete cases
  include <- lapply(res, "[[", "coef") %>% do.call(rbind, .) %>% complete.cases
  
  
  # Computing bias
  ci   <- lapply(res, "[[", "vcov")[include]
  
  # Obtaining confidence intervals
  ci_edges  <- lapply(ci, "[", i="edges", j=,drop=FALSE) %>% do.call(rbind, .)
  ci_mutual <- lapply(ci, "[", i="mutual", j=,drop=FALSE) %>% do.call(rbind, .)
  
  # Obtaining parameters
  pars <- lapply(dgp, "[[", "par")[include] %>% do.call(rbind, .)
  size <- lapply(dgp, "[[", "size")[include] %>% do.call(rbind, .) %>% rowSums
  
  # Looking at power

  # Edges
  edges <- ((ci_edges[,1] > 0) & (ci_edges[,2] > 0)) |
    ((ci_edges[,1] < 0) & (ci_edges[,2] < 0))
  
  edges[edges] <- ifelse(pars[edges, 1L] > 0, ci_edges[edges,1] > 0, ci_edges[edges,1] < 0)
  
  # Mutual
  mutual <- ((ci_mutual[,1] > 0) & (ci_mutual[,2] > 0)) |
    ((ci_mutual[,1] < 0) & (ci_mutual[,2] < 0))
  
  mutual[mutual] <- ifelse(pars[mutual, 2L] > 0, ci_mutual[mutual,1] > 0, ci_mutual[mutual,1] < 0)
  
  # Generating tags
  power_edges <- dplyr::tibble(
    `Sample size` = interval_tags(size, intervals),
    `Effect Size` = interval_tags(abs(pars[, 1]), intervals_effect),
    power         = edges
  ) %>% dplyr::group_by(`Sample size`, `Effect Size`) %>%
    dplyr::summarise(
      Power   = mean(power, na.rm=TRUE),
      N       = n()
      )
  
  power_mutual <- dplyr::tibble(
    `Sample size` = interval_tags(size, intervals),
    `Effect Size` = interval_tags(abs(pars[, 2]), intervals_effect),
    power = mutual
  ) %>% dplyr::group_by(`Sample size`, `Effect Size`) %>%
    dplyr::summarise(
      Power   = mean(power, na.rm=TRUE),
      N       = n()
    )
  
  # Writing table output
  capture.output({
    pander::pandoc.table(
      power_edges,
      digits = 2,
      caption = names(experiments)[i],
      keep.trailing.zeros = TRUE
    )
  }, file = sprintf("simulations/power-edges-%s.md", e))
  
  capture.output({
    pander::pandoc.table(
      power_mutual,
      digits = 2,
      caption = names(experiments)[i],
      keep.trailing.zeros = TRUE
      )
  }, file = sprintf("simulations/power-mutual-%s.md", e))
  
  
  # Binding and plotting
  dat <- rbind(
    cbind(power_mutual, Parameter = rep("Mutual", nrow(power_mutual))),
    cbind(power_edges, Parameter  = rep("Edges", nrow(power_edges)))
    ) %>%
    subset(!is.na(`Effect Size`)) %>%
    dplyr::mutate(
      `Effect Size` = paste("Effect size in", `Effect Size`)
    )
  
  set.seed(1)
  ggplot(dat, aes(x = `Sample size`, y = Power, colour = Parameter)) +
    facet_wrap(~ `Effect Size`, nrow = 2, ncol = 2) + 
    scale_color_grey() +
    geom_line(aes(group=Parameter), linetype=2) +
    geom_point(aes(size=N)) +
    xlab(paste("# of networks per sample", sprintf("(samples included = %d)", sum(include)))) +
    ylab("Empirical Power") + 
    ylim(0, 1) + 
    theme_light() +
    # annotate(
    #   "text", x = 1.5, y=.3 ,
    #   label = sprintf("# of samples included %d", sum(include)),
    #   family = "Times"
    #   ) +
    theme(
      text = element_text(family = "AvantGarde"),
      axis.text.x = element_text(angle = 45)
      ) +
    ggsave(
      sprintf("simulations/power-%s.pdf", e),
      width = 8*.8, height = 6*.8
    )
  
}



