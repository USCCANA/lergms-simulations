library(data.table)
res <- readRDS("simulations/02-various-sizes-4-5-ttriad.rds")

# Needed to be rerun
rerun <- sapply(res, function(r) {
  if (inherits(r$rm, "error"))
    return("error")
  if (length(r$rm$time0))
    return("second")
  "first"
})

types <- names(res[[1]])

errors <- vector("list", length(types))
for (i in seq_along(types)) {
  
  # Extracting
  errors[[i]] <- lapply(res, "[[", types[i])
  
  # Subsetting errors
  errors[[i]] <- errors[[i]][sapply(errors[[i]], inherits, what = "error")]
  
  # Parsing errors
  if (length(errors[[i]])) {
    
    errors[[i]] <- sapply(errors[[i]], as.character)
    errors[[i]] <- sapply(errors[[i]], gsub, pattern = ".+[:]", replacement = "")
    
  }
  
}

errors_tab <- NULL
for (i in seq_along(errors)) {
  
  if (!length(errors[[i]]))
    next
  
  errors_tab <- as.data.frame(table(errors[[i]]))
}
lapply(errors[-1], table)
