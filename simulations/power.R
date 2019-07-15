library(ggplot2)
library(magrittr)

source("simulations/interval_tags.R")
intervals        <- c(1, 20, 30, 40, 50, 100)
intervals_effect <- c(.2, .5, 1, 2, 2.5)

experiments <- c(
   # "Distributio of Power by Sample size (Networks of size 4)" = "01-fixed-sizes-4",
   "(Networks of size 3 and 5)" = "02-various-sizes-3-5" #,
   # "Distributio of Power by Sample size (Networks of size 4, bootstrap CI)" = "01-fixed-sizes-4-boot",
   # "Distributio of Power by Sample size (Networks of size 3 and 4, bootstrap CI)" = "02-various-sizes-3-4-boot"
)


for (i in seq_along(experiments)) {
  
  e <- experiments[i]
  
  # Reading data
  # res <- readRDS(sprintf("simulations/%s.rds", e))
  dgp <- readRDS(sprintf("simulations/%s-dat.rds", gsub("-boot$", "", e)))
  
  for (m in c("ergm", "ergmito")) {
  
    res <- lapply(readRDS(sprintf("simulations/%s.rds", e)), "[[", m)
    
    nsim <- length(res)
    
    # Complete cases
    # Droping the Inf
    include <- lapply(res, "[[", "coef") %>% do.call(rbind, .)
    include[!is.finite(include)] <- NA
    include <- complete.cases(include)
    
    # Computing bias
    ci   <- lapply(res, "[[", "ci")[include]
    
    # Obtaining confidence intervals
    ci_edges  <- lapply(ci, "[", i="edges", j=,drop=FALSE) %>% do.call(rbind, .)
    ci_mutual <- lapply(ci, "[", i="mutual", j=,drop=FALSE) %>% do.call(rbind, .)
    
    # Obtaining parameters
    pars <- lapply(dgp, "[[", "par")[include] %>% do.call(rbind, .)
    size <- lapply(dgp, "[[", "size")[include] %>% do.call(rbind, .) %>% rowSums
    
    # Coverage probability
    ids_cov_edges <- sign(ci_edges[, 1L]) == sign(ci_edges[, 2L])
    edges_cov <- (ci_edges[ids_cov_edges,1L] <= pars[ids_cov_edges,1L]) & (ci_edges[ids_cov_edges,2L] >= pars[ids_cov_edges,1L])
    ids_cov_mutual <- sign(ci_mutual[, 1L]) == sign(ci_mutual[, 2L])
    mutual_cov <-(ci_mutual[ids_cov_mutual,1L] <= pars[ids_cov_mutual,2L]) & (ci_mutual[ids_cov_mutual,2L] >= pars[ids_cov_mutual,2L])
    
    # Looking at power
  
    # Edges
    edges <-
      (sign(ci_edges[,1]) == sign(ci_edges[,2])) & # Significant 
      (sign(ci_edges[,1]) == sign(pars[,1L]))      # In the right direction

    # Mutual
    mutual <-
      (sign(ci_mutual[,1]) == sign(ci_mutual[,2])) & # Significant 
      (sign(ci_mutual[,1]) == sign(pars[,2L]))      # In the right direction
    
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
    
    # Coverage
    coverage_edges <- dplyr::tibble(
      `Sample size` = interval_tags(size, intervals)[ids_cov_edges],
      `Effect Size` = interval_tags(abs(pars[, 1]), intervals_effect)[ids_cov_edges],
      power         = edges_cov
    ) %>% dplyr::group_by(`Sample size`, `Effect Size`) %>%
      dplyr::summarise(
        Coverage = mean(power, na.rm=TRUE),
        N        = n()
      )
    
    coverage_mutual <- dplyr::tibble(
      `Sample size` = interval_tags(size, intervals)[ids_cov_mutual],
      `Effect Size` = interval_tags(abs(pars[, 2]), intervals_effect)[ids_cov_mutual],
      power = mutual_cov
    ) %>% dplyr::group_by(`Sample size`, `Effect Size`) %>%
      dplyr::summarise(
        Coverage = mean(power, na.rm=TRUE),
        N        = n()
      )
    
    # Writing table output
    capture.output({
      pander::pandoc.table(
        power_edges,
        digits = 2,
        caption = names(experiments)[i],
        keep.trailing.zeros = TRUE
      )
    }, file = sprintf("simulations/power-edges-%s-%s.md", e, m))
    
    capture.output({
      pander::pandoc.table(
        power_mutual,
        digits = 2,
        caption = names(experiments)[i],
        keep.trailing.zeros = TRUE
        )
    }, file = sprintf("simulations/power-mutual-%s-%s.md", e, m))
    
    
    # Binding and plotting - POWER ---------------------------------------------
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
      geom_point(aes(size = N)) +
      xlab("# of networks per sample") +
      ylab("Empirical Power") + 
      ylim(0, 1) + 
      theme_light() +
      labs(
        title    = paste("Empirical Power", names(experiments)[i], "fitted using", m),
        subtitle = sprintf("# of observations %d (%d discarded due to Inf)", sum(include), sum(!include))
      ) +
      theme(
        text = element_text(family = "AvantGarde"),
        axis.text.x = element_text(angle = 45)
        ) +
      ggsave(
        sprintf("simulations/power-%s-%s.pdf", e, m),
        width = 8*.8, height = 6*.8
      )
    
    # Binding and plotting - COVERAGE ------------------------------------------
    dat <- rbind(
      cbind(coverage_mutual, Parameter = rep("Mutual", nrow(coverage_mutual))),
      cbind(coverage_edges, Parameter  = rep("Edges", nrow(coverage_edges)))
    ) %>%
      subset(!is.na(`Effect Size`)) %>%
      dplyr::mutate(
        `Effect Size` = paste("Effect size in", `Effect Size`)
      )
    
    set.seed(1)
    ggplot(dat, aes(x = `Sample size`, y = Coverage, colour = Parameter)) +
      facet_wrap(~ `Effect Size`, nrow = 2, ncol = 2) + 
      scale_color_grey() +
      geom_line(aes(group=Parameter), linetype=2) +
      geom_point(aes(size = N)) +
      xlab("# of networks per sample") +
      ylab("Empirical Coverage") + 
      ylim(0, 1) + 
      theme_light() +
      labs(
        title    = paste("Empirical Coverage", names(experiments)[i], "fitted using", m),
        subtitle = sprintf("# of observations %d (%d discarded due to Inf)", sum(include), sum(!include))
      ) +
      theme(
        text = element_text(family = "AvantGarde"),
        axis.text.x = element_text(angle = 45)
      ) +
      ggsave(
        sprintf("simulations/coverage-%s-%s.pdf", e, m),
        width = 8*.8, height = 6*.8
      )
  }
}



