library(sna)
library(ergmito)
load("data/model_data.rda")
net <- NETWORKS$advice

# Removing cases with missing
net <- net[which(!(as.integer(names(net)) %in% as.integer(miss$Female)))]
net <- net[nvertex(net) > 3]

# Descriptive stats, in particular, a prop.test

females <- sapply(net, function(i) sum(get.vertex.attribute(i, "Female")))
males   <- nvertex(net) - females

prop.test(females, nvertex(net), correct = TRUE)
range(unlist(Map(function(a,b) binom.test(a,b)$p.value, a = females, b = nvertex(net))))

mean(females/nvertex(net))

# quaptest <- parallel::mclapply(1:1000, function(i) {
#   net0 <- net[[1]]
#   set.vertex.attribute(
#     net0,
#     "Female",
#     sample(get.vertex.attribute(net0, "Female"))
#     )
#   summary(net0 ~ nodematch("Female"))[1]
# }, mc.cores = 4)

# Generating a sample
sample_gen <- function(net, n = 100) {
  # Storing the vector of relevant attributes
  female_attrs <- get.vertex.attribute(net, attrname = "Female")
  
  stats_obs <- dyad.census(net)
  
  # Generating the random sample
  dat <- rguman(
    n, nv  = nvertex(net),
    mut    = stats_obs[,"Mut"],
    asym   = stats_obs[,"Asym"],
    null   = stats_obs[,"Null"],
    method = "exact"
  )
  
  ans <- vector("list", n)
  for (i in seq_along(ans)) {
    
    ans[[i]] <- network(dat[i, , ], vertex.attr = list(Female = female_attrs))
    
  }
  
  count_stats(ans ~ nodematch("Female"))
}

# Generating 100 samples per network
set.seed(1231)
cug_samples <- parallel::mclapply(net, sample_gen, n = 200, mc.cores = 4L)
cug_samples <- do.call(cbind, cug_samples)

# Retrieving the ids to draw from each case
idx <- replicate(nnets(net), sample.int(200, 5000, TRUE), simplify = FALSE)
idx <- do.call(cbind, idx)

# Computing the average nodematch("Female")
null_dist <- apply(idx, 1, function(i) mean(cug_samples[cbind(i, 1L:31L)]))

# Observed average
obs <- mean(count_stats(net ~ nodematch("Female")))

# Density plot
graphics.off()
pdf("analysis/ci-cug-test.pdf", width = 5, height = 5)
op <- par(mar = par("mar") * c(1, 1, .2, .2))
plot(
  density(null_dist),
  xlab = "Ave. # of homofilic ties on gender",
  main = ""
)
abline(v = obs, lty = 2, lwd = 2)
text(
  x = obs, y = .25,
  labels = sprintf("Avg. # observed in \nthe data: %.2f.", obs),
  pos = 4)
par(op)
dev.off()

