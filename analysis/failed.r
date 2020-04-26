library(ggplot2)
library(magrittr)
library(data.table)
library(xtable)
library(ergmito)

source("sim-figures/interval_tags.R")
intervals_effect <- c(.1, .5, 1, 2)

experiments <- c(
  # "Distribution of Empirical Bias (mutual)" = "02-various-sizes-4-5-mutual",
  "Distribution of Empirical Bias (ttriad)" = "02-various-sizes-4-5-ttriad"
)

e <- experiments[1]
term_name <- c("edges", gsub(".+[-](?=[a-zA-Z]+$)", "", e, perl = TRUE))

# Reading data
# res  <- readRDS(sprintf("simulations/%s.rds", e))
res <- readRDS("simulations/02-various-sizes-4-5-ttriad.rds")
dgp  <- readRDS("simulations/dgp_4_5_ttriad.rds")
pars <- lapply(dgp, "[[", "par")

# Generating definitions

for (full in c(TRUE)) {
  # Fail to compute (Error) ------------------------------------------------------
  
  if (full) {
    fitted_common <- 1:length(res)
  } else {
    # Alternatively, we are defining the common set to that in which the graphs are
    # not fully connected nor are empty
    fitted_common <- sapply(dgp, function(d) {
      n <- nvertex(d$nets)
      d <- count_stats(d$nets ~ edges + ttriad)
      any(d[,2] > 0) & any(d[,1] < (n*(n-1)))
    })
    
    fitted_common <- which(fitted_common)
    nfitted <- length(fitted_common)
  }
  
  # Listing failed ones
  failed_mcmle <- lapply(res[fitted_common], "[[", "mcmle")
  failed_mcmle <- sapply(failed_mcmle, inherits, what = "error")
  
  failed_mle <- lapply(res[fitted_common], "[[", "mle")
  failed_mle <- sapply(failed_mle, inherits, what = "error")
  
  failed_rm <- lapply(res[fitted_common], "[[", "rm")
  failed_rm <- sapply(failed_rm, inherits, what = "error") 
  
  overall <- table(failed_mcmle, failed_mle)
  
  # Failures as a function of sample size ----------------------------------------
  sampsize <- sapply(dgp[fitted_common], function(d) nnets(d$nets))
  
  failures <- data.table(
    Size   = sampsize,
    Model  = c(rep("MC-MLE", length(sampsize)), rep("MLE", length(sampsize)), rep("RM", length(sampsize))), 
    Failed = c(failed_mcmle, failed_mle, failed_rm)
  )
  
  failures <- failures[, list(ErrRate = mean(Failed)),by=c("Model", "Size")]
  
  failures <- cbind(failures[Model=="MLE", ], failures[Model=="MC-MLE"]$ErrRate, failures[Model=="RM"]$ErrRate)
  failures$Model <- NULL
  names(failures) <- c("Sample size", "MLE", "MC-MLE", "RM")
  
  tab <- xtable(
    failures, 
    caption = "\\label{tab:error-sampsize}Error probability my method and sample size.",
    )
  align(tab) <- c("l", "l", "c", "c", "c")
  print(
    tab,
    booktabs = TRUE,
    file = "analysis/failed_by_size.tex",
    include.rownames = FALSE
  )
  
  tab <- readLines("analysis/failed_by_size.tex")
  tab[grepl("\\\\toprule", tab)] <- 
    "\\toprule & \\multicolumn{3}{c}{P(Error)} \\\\ \\cmidrule(r){2-4}"
  writeLines(tab, "analysis/failed_by_size.tex")
}
