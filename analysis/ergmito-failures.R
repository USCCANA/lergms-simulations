library(ggplot2)
library(magrittr)
library(data.table)
library(xtable)

source("simulations/interval_tags.R")
intervals_effect <- c(.1, .5, 1, 2)

experiments <- c(
  # "Distribution of Empirical Bias (mutual)" = "02-various-sizes-4-5-mutual",
  "Distribution of Empirical Bias (ttriad)" = "02-various-sizes-4-5-ttriad"
)

e <- experiments[1]
term_name <- c("edges", gsub(".+[-](?=[a-zA-Z]+$)", "", e, perl = TRUE))

# Reading data
res  <- readRDS(sprintf("simulations/%s.rds", e))
dgp  <- readRDS(sprintf("simulations/%s-dat.rds", e))
pars <- lapply(dgp, "[[", "par")

# Tagging ergmito failures -----------------------------------------------------
library(ergmito)
failed <- sapply(res, function(i) {
  inherits(i$ergmito, "error")
}) %>% which

dgp_failed <- dgp[failed]
rm(dgp)
gc()

lapply(res[failed], "[[", "ergmito") %>%
  sapply("[[", "message")

res_new <- lapply(dgp_failed, function(d) {
  tryCatch(ergmito(d$net ~ edges + ttriad), error=function(e) e)
})

sapply(res_new, "[[", "message")
