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
  
  failures <- failures[, list(ErrRate = sum(Failed)),by=c("Model", "Size")]
  
  failures <- cbind(failures[Model=="MLE", ], failures[Model=="MC-MLE"]$ErrRate, failures[Model=="RM"]$ErrRate)
  failures$Model <- NULL
  names(failures) <- c("Sample size", "MLE", "MC-MLE", "RM")
  failures <- rbind(
    failures,
    with(
      failures,
      data.table(
        `Sample size` = "\\midrule Total",
        MLE           = sum(`MLE`),
        `MC-MLE`      = sum(`MC-MLE`),
        RM            = sum(RM)
        )
      )
  )
  
  tab <- xtable(
    failures, 
    caption = "\\label{tab:error-sampsize}Number of times the program failed to fit a model and returned with an error.",
    )
  align(tab) <- c("l", "l", "c", "c", "c")
  print(
    tab,
    booktabs = TRUE,
    file = "analysis/failed_by_size.tex",
    include.rownames = FALSE, 
    sanitize.text.function = function(e) e
  )
  
  tab <- readLines("analysis/failed_by_size.tex")
  tab[grepl("\\\\toprule", tab)] <- 
    "\\toprule & \\multicolumn{3}{c}{\\# of errors)} \\\\ \\cmidrule(r){2-4}"
  writeLines(tab, "analysis/failed_by_size.tex")
}

# Looking a the space of failures ----------------------------------------------

# Listing failed ones
conv_mle <- sapply(res, function(r) {
  if (inherits(r$mle, "error"))
    return(FALSE)
  r$mle$status == 0
})

conv_mcmle <- sapply(res, function(r) {
  if (inherits(r$mcmle, "error"))
    return(FALSE)
  !r$mcmle$degeneracy
})

conv_rm <- sapply(res, function(r) {
  if (inherits(r$rm, "error"))
    return(FALSE)
  !r$rm$degeneracy
})



# Failed cases
mat4 <- matrix(1, ncol = 4, nrow = 4)
diag(mat4) <- 0
mat5 <- matrix(1, ncol = 5, nrow = 5)
diag(mat5) <- 0

maxref <- matrix(ncol=2, nrow=5)
maxref[4,] <- count_stats(mat4 ~ edges + ttriad)
maxref[5,] <- count_stats(mat5 ~ edges + ttriad)

stats <- parallel::mclapply(lapply(dgp, "[[", "nets"), count_stats, terms = c("edges", "ttriad"), mc.cores = 3L)
sizes_failed <- parallel::mclapply(lapply(dgp, "[[", "nets"), nvertex, mc.cores = 3L)
stats <- parallel::mcMap(function(x, s) {
  
  # Normalizing to the size
  colMeans(x/maxref[s,])
  
}, x = stats, s = sizes_failed, mc.cores = 4L)
stats <- as.data.frame(do.call(rbind, stats))
colnames(stats) <- c("edges", "ttriad")

# Creating a supper set
dat <- rbind(
  cbind(stats[which(conv_mle),], Method = "MLE", Failed = "Converge"),
  cbind(stats[which(!conv_mle),], Method = "MLE", Failed = "Did not converge"),
  cbind(stats[which(conv_mcmle),], Method = "MC-MLE", Failed = "Converge"),
  cbind(stats[which(!conv_mcmle),], Method = "MC-MLE", Failed = "Did not converge"),
  cbind(stats[which(conv_rm),], Method = "RM", Failed = "Converge"),
  cbind(stats[which(!conv_rm),], Method = "RM", Failed = "Did not converge")
)

dat <- as.data.table(dat)
set.seed(12315)
idx <- sort(sample.int(nrow(dat)/3, nrow(dat)/3*.4))
dat[, keep := 1:.N %in% idx, by = Method]
dat[, keep := keep | ((abs(.5 - edges) <= .4) & (abs(.5 - ttriad) <= .4)), by = Method]

ggplot(dat, aes(x=edges, y=ttriad)) +
  facet_grid(Method ~ Failed) +
  theme_bw() +
  # lims(x = c(0,1), y=c(0,1)) +
  geom_point(
    size=.75,
    aes(x = edges, y = ttriad),
    data = dat[keep== TRUE],
    color = adjustcolor("darkgray", .7)
    ) +
  # geom_jitter(color = adjustcolor("gray", .7))
  stat_density2d(
    geom="tile",
    aes(fill=..density..^0.75, alpha=1),
    contour =FALSE,
    h = c(.2, .2), n = 40,
    ) + 
  scale_fill_gradientn(colours = colorRampPalette(c("white", "black"))(256)) +
  theme(
    text        = element_text(family = "AvantGarde"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.position = "none" 
  )

ggsave("sim-figures/failed.pdf", width = 5, height = 6)
  

