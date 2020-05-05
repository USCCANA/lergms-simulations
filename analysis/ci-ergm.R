library(ergmito)
load("data/model_data.rda")
net <- NETWORKS$advice

# Removing cases with missing
net <- net[which(!(as.integer(names(net)) %in% as.integer(miss$Female)))]
net <- net[nvertex(net) > 3]

# Examples for the paper
f <- ergmito_formulae(
  net ~ edges + ttriad,
  model_update = ~. +
    I(edges * (n == 5)) +
    I(ttriple * (n == 5)) +
    I(edges * log(1/n))
  )

d <- data.table::data.table(f$target_stats)
d$n <- as.vector(nvertex(net))
d[, first:=1:.N < 4,by=n]
d <- d[first == TRUE]
d$first <- NULL
colnames(d) <- c("edges", "ttriad", "e*n", "t*n", "offset", "n")
d <- d[, c("n", "edges", "ttriad", "e*n", "t*n", "offset")]

print(
  xtable::xtable(
    d,
    display = c("d", "d", "d", "d", "d", "d", "f")
    ),
  include.rownames = FALSE
  )

# Examples (2) for the paper
f <- ergmito_formulae(
  net ~ edges + nodematch("Female") + nodeicov("Female") + nodeocov("Female"),
  model_update = ~. +
    I(sqrt(nodematch.Female)) + 
    I((nodematch.Female)^(1.25))
)

d <- data.table::data.table(f$target_stats)
d$n <- as.vector(nvertex(net))
d[, first:=1:.N < 4,by=n]
d <- d[first == TRUE]
d$first <- NULL
# colnames(d) <- c("edges", "ttriad", "e*n", "t*n", "offset", "n")
d<-data.frame(d, check.names = FALSE)
d <- cbind(n = d[,ncol(d)], d[,-c(1, ncol(d))])
colnames(d) <- c(
  "$n$",
  "Homophily (gender)", "Receiver (female)", "Sender (female)",
  "$\\text{Homophily}^{1/2}$", "$\\text{Homophily}^{2}$"
  )
d <- xtable::xtable(d, display = c("d", "d", "d", "d", "d", "f", "f"))
xtable::align(d) <- rep("\\m{.1\\linewidth}<\\centering", 7)
print(
  d,
  include.rownames = FALSE,
  sanitize.text.function = function(i) i,
  booktabs = TRUE
)

f$target_stats
# Part 1: Structural based models ----------------------------------------------
net_baseline00 <- ergmito(net ~ edges + ttriad)
net_baseline01 <- ergmito(net ~ edges + ttriad, model_update = ~ . + offset(I(edges * log(1/n))))
net_baseline02 <- ergmito(net ~ edges + ttriad, model_update = ~ . + I(edges * (n == 5)))
net_baseline03 <- ergmito(net ~ edges + ttriad, model_update = ~ . + I(ttriple * (n == 5)))
net_baseline04 <- ergmito(net ~ edges + ttriad, model_update = ~ . + I(ttriple * (n == 5)) + I(edges * (n == 5)))
net_baseline02_bs <- ergmito_boot(net_baseline02, R = 1000)

texreg::texreg(
  list(
    `(1)` = net_baseline00,
    `(2)` = net_baseline01,
    `(3)` = net_baseline02,
    `(4)` = net_baseline03,
    `(5)` = net_baseline04,
    `(3b)` = net_baseline02_bs
  ),
  single.row   = FALSE,
  include.convergence = FALSE,
  reorder.coef = c(1,2,4,5,3),
  caption      = paste(
    "Structural models. Model (2) includes Krivitsky et al (2011) offset term.",
    "Besides of the common GOF statistics, the table includes the number of",
    "networks used, elapsed time to fit the model, and, in the case of Model",
    "(3b) which is a bootstrapped version of model (3), number of replicates fitted",
    "and included in the bootstrap variance estimate."
  ),
  label = "tab:ci-ergm-baseline",
  file = "analysis/ci-ergm-baseline.tex",
  float.pos = "tb",
  booktabs = TRUE,
  use.packages = FALSE
)

graphics.off()
pdf("analysis/ci-ergmito-gof-baseline.pdf", width = 4, height = 3.6)
op <- par(mar = par("mar") * c(1, 1, .25, .5))
plot(gof_ergmito(net_baseline02), sub = "", main = "", xlab = "")
par(op)
dev.off()

plot(net_baseline02)

# Part 2: Main hypothesis testing ----------------------------------------------

# Arbitrary models: Interaction term with a function of size
net_struct_baseline01 <- ergmito(
  net ~ edges + ttriad + nodematch("Female"),
  model_update = ~ . + I(edges * (n == 5))
)

# net_struct_baseline01 <- ergmito(
#   net ~ edges + ttriad + nodematch("Female"),
#   model_update = ~ . + I(edges * (n == 5)) + I(nodematch.Female * (n == 5))
# )

net_struct_baseline02 <- ergmito(
  net ~ edges + ttriad + nodematch("Female"),
  model_update = ~ . + I(edges * (n == 5)) + I(sqrt(nodematch.Female))
)

net_struct_baseline03 <- ergmito(
  net ~ edges + ttriad + nodematch("Female"),
  model_update = ~ . + I(edges * (n == 5)) + offset(I(ifelse(edges <= 4, -Inf, 0)))
) #;net_struct_baseline03$optim.out

# Calculating the CDF with and without the offset ------------------------------

edges_seq <- sort(unique(net_struct_baseline01$formulae$stats_statmat[[1]][, "edges"]))

# With the offset term
cdf_with_offset <- with(net_struct_baseline03$formulae, {

  sapply(edges_seq, function(e) {
    
    idx <- which(stats_statmat[[1]][, "edges"] == e)
    
    if (length(idx) == 0)
      return(0)
    
    p <- exp(exact_loglik(
      x             = stats_statmat[[1]][idx, , drop = FALSE],
      params        = coef(net_struct_baseline03),
      stats_weights = stats_weights[[1]],
      stats_statmat = stats_statmat[[1]],
      target_offset = stats_offset[[1]][idx],
      stats_offset  = stats_offset[[1]]
    ))
    
    sum(p * stats_weights[[1]][idx])
  })
  
  
})


cdf_without_offset <- with(net_struct_baseline01$formulae, {

  sapply(edges_seq, function(e) {
    
    idx <- which(stats_statmat[[1]][, "edges"] == e)
    
    if (length(idx) == 0)
      return(0)
    
    p <- exp(exact_loglik(
      x      = stats_statmat[[1]][idx, , drop = FALSE],
      params = coef(net_struct_baseline01),
      stats_weights = stats_weights[[1]],
      stats_statmat = stats_statmat[[1]],
      target_offset = stats_offset[[1]][idx],
      stats_offset = stats_offset[[1]]
    ))
    
    sum(p * stats_weights[[1]][idx])
  })
  
  
})

plot(cumsum(cdf_with_offset), type = "l", col = "red", lwd=2)
lines(cumsum(cdf_without_offset), col = "blue", lwd=2)


net_struct_baseline04 <- ergmito(
  net ~ edges + ttriad + nodematch("Female") + nodeocov("Female"),
  model_update = ~ . + I(edges * (n == 5))
)

net_struct_baseline05 <- ergmito(
  net ~ edges + ttriad + nodematch("Female") + nodeicov("Female"),
  model_update = ~ . + I(edges * (n == 5))
)

net_struct_baseline04_bs <- ergmito_boot(net_struct_baseline04, R = 1000)

texreg::texreg(
  list(
    `(1)` = net_struct_baseline01,
    `(2)` = net_struct_baseline02,
    `(3)` = net_struct_baseline03,
    `(4)` = net_struct_baseline04,
    `(5)` = net_struct_baseline05,
    `(4b)` = net_struct_baseline04_bs
  ),
  single.row          = FALSE,
  include.convergence = FALSE,
  label               = "tab:ci-ergm-full",
  file                = "analysis/ci-ergm-full.tex",
  caption             = paste(
    "Testing for gender homopholy. Models (1) through (3) include either an",
    "interaction term or a transformation of the term nodematch(\"Female\").",
    "Models (4) and (5) include sender and receiver effects, while model (4b)",
    "is a bootstrapped version of model (4)."
  ),
  float.pos = "tb",
  booktabs = TRUE,
  use.packages = FALSE
)


net_struct_baseline04

graphics.off()
pdf("analysis/ci-ergmito-gof-full.pdf", width = 4, height = 7)
op <- par(mar = par("mar") * c(1, 1, .2, .5))
plot(gof_ergmito(net_struct_baseline04), sub = "", main = "")
par(op)
dev.off()

