library(ggplot2)
library(magrittr)

source("simulations/interval_tags.R")

experiments <- c(
   "Distributio of Power by Sample size (Networks of size 4)" = "01-fixed-sizes-4",
   "Distributio of Power by Sample size (Networks of size 3 and 4)" = "02-various-sizes-3-4",
   "Distributio of Power by Sample size (Networks of size 4, bootstrap CI)" = "01-fixed-sizes-4-boot",
   "Distributio of Power by Sample size (Networks of size 3 and 4, bootstrap CI)" = "02-various-sizes-3-4-boot"
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
    `Sample size` = interval_tags(size, c(0, 10, 20, 30, 40, 50, 60, +Inf)),
    power = edges
  ) %>% dplyr::group_by(`Sample size`) %>%
    dplyr::summarise(
      Power   = mean(power, na.rm=TRUE),
      N       = n()
      )
  
  power_mutual <- dplyr::tibble(
    `Sample size` = interval_tags(size, c(0, 10, 20, 30, 40, 50, 60, +Inf)),
    power = mutual
  ) %>% dplyr::group_by(`Sample size`) %>%
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
    cbind(power_mutual, Parameter = "Mutual"),
    cbind(power_edges, Parameter = "Edges")
    )
  
  set.seed(1)
  ggplot(dat, aes(x = `Sample size`, y = Power, colour = Parameter)) +
    geom_jitter(aes(size=N), width = .1, height = 0) +
    xlab("Sample size") +
    ylim(0, 1) + 
    labs(
      title    = names(experiments)[i],
      subtitle = sprintf("# of observations %d", sum(include))
    ) + ggsave(
      sprintf("simulations/power-%s.pdf", e),
      width = 8, height = 6
    )
  
}



