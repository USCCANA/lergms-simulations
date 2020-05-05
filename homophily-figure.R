# Figures: Why does it make sense to include the square term?

library(ergmito)
library(network)
library(sna)
load("data/model_data.rda")
net <- NETWORKS$advice

# Removing cases with missing
net <- net[which(!(as.integer(names(net)) %in% as.integer(miss$Female)))]
net <- net[nvertex(net) > 3]

# Looking at the expected number of homophilic ties
n0 <- structure(
  c(0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0),
  .Dim = c(5L, 5L),
  .Dimnames = list(c("A",  "B", "C", "D", "E"), c("A", "B", "C", "D", "E"))
  )

n0 <- network(n0)
set.vertex.attribute(n0, "Female", c(0L, 0L, 1L, 1L, 0L))
# gplot(n0, vertex.col = get.vertex.attribute(n0, "Female"))

f <- ergmito_formulae(
  n0 ~ edges + nodematch("Female"),
  model_update = ~ . + I(nodematch.Female^(1.25)) + I(nodematch.Female^(.5))
  )

# Computing likelihoods
thetas <- seq(-5, 5, length.out = 100)
theta_par0 <- .5

# Nodematch
nm <- sapply(thetas, function(t.) {
  # Calculating over all the possible states
  p <- exact_loglik(
    x = f$stats_statmat[[1]],
    params = c(-.5, theta_par0, 0, 0),
    stats_weights = f$stats_weights,
    stats_statmat = f$stats_statmat
    )
  
  # Computing the expected values
  sum(exp(p) * f$stats_weights[[1]] * f$stats_statmat[[1]][,1])
})

# Nodematch
nm_squared <- sapply(thetas, function(t.) {
  # Calculating over all the possible states
  p <- exact_loglik(
    x = f$stats_statmat[[1]],
    params = c(-.5, theta_par0, t., 0),
    stats_weights = f$stats_weights,
    stats_statmat = f$stats_statmat
  )
  
  # Computing the expected values
  sum(exp(p) * f$stats_weights[[1]] * f$stats_statmat[[1]][,2])
})

nm_sqrt <- sapply(thetas, function(t.) {
  # Calculating over all the possible states
  p <- exact_loglik(
    x = f$stats_statmat[[1]],
    params = c(-.5, theta_par0, 0, t.),
    stats_weights = f$stats_weights,
    stats_statmat = f$stats_statmat
  )
  
  # Computing the expected values
  sum(exp(p) * f$stats_weights[[1]] * f$stats_statmat[[1]][,2])
})

plot(
  x = thetas, y = nm, type = "l",
  ylim = range(f$stats_statmat[[1]][,2]),
  lwd = 2, lty = 1,
  xlab = expression(plain(Values~of)~theta),
  ylab = c("Expected count of","homophilic (gender) ties")
  )
lines(x = thetas, y =nm_sqrt, lwd = 2, col = "blue", lty = 2)
lines(x = thetas, y =nm_squared, lwd = 2, col = "red", lty = 3)

legend(
  "topleft",
  legend = expression(
    Homophily,
    Homophily^(1/2),
    Homophily^2
  ),
  lty = 1L:3L, lwd = 2,
  col = c("black", "blue", "red"),
  bty = "n"
)


# Calculating probabilities of observing certain figures

counts <- sort(unique(f$stats_statmat[[1]][,2]))
theta_par  <- -.5


# Nodematch
nm <- sapply(counts, function(t.) {
  # Calculating over all the possible states
  p <- exact_loglik(
    x = f$stats_statmat[[1]],
    params = c(-.5, theta_par0, 0, 0),
    stats_weights = f$stats_weights,
    stats_statmat = f$stats_statmat
  )
  
  # Computing the expected values
  idx <- which(f$stats_statmat[[1]][,2] == t.)
  sum(exp(p[idx]) * f$stats_weights[[1]][idx])
})

# Nodematch
nm_squared <- sapply(counts, function(t.) {
  # Calculating over all the possible states
  p <- exact_loglik(
    x = f$stats_statmat[[1]],
    params = c(-.5, theta_par0, theta_par, 0),
    stats_weights = f$stats_weights,
    stats_statmat = f$stats_statmat
  )
  
  # Computing the expected values
  idx <- which(f$stats_statmat[[1]][,2] == t.)
  sum(exp(p[idx]) * f$stats_weights[[1]][idx])
})

nm_sqrt <- sapply(counts, function(t.) {
  # Calculating over all the possible states
  p <- exact_loglik(
    x = f$stats_statmat[[1]],
    params = c(-.5, theta_par0, 0, theta_par),
    stats_weights = f$stats_weights,
    stats_statmat = f$stats_statmat
  )
  
  # Computing the expected values
  idx <- which(f$stats_statmat[[1]][,2] == t.)
  sum(exp(p[idx]) * f$stats_weights[[1]][idx])
})

plot(
  x = counts, y = nm, type = "l",
  ylim = c(0, .4),
  lwd = 2, lty = 1,
  xlab = c("Count of homophilic (gender) ties"),
  ylab = c("Marginal CDF for","homophilic (gender) ties")
)
lines(x = counts, y =nm_sqrt, lwd = 2, col = "blue", lty = 2)
lines(x = counts, y =nm_squared, lwd = 2, col = "red", lty = 3)

legend(
  "topleft",
  legend = expression(
    Homophily,
    Homophily^(1/2),
    Homophily^2
  ),
  lty = 1L:3L, lwd = 2,
  col = c("black", "blue", "red"),
  bty = "n"
)

legend(
  "topright",
  legend = expression(
    plain(All~values~of)~theta==0.5,
    theta[plain(edges)]==-0.5
    ),
  bty = "n"
)

