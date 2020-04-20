# Creates nice interval tags in the form of [a,b)...[a,z] (last one closed).
# All numbers must be within the interval
interval_tags <- function(x, marks) {
  
  # Are marks integers?
  
  marks0 <- marks[is.finite(marks)]
  if (sum(marks0 - floor(marks0)) < 1e-20)
    mask <- c("[%.0f, %.0f)", "[%.0f, %.0f]")
  else
    mask <- c("[%.1f, %.1f)", "[%.1f, %.1f]")
  
  # Generating labels
  n <- length(marks)
  l <- c(sprintf(mask[1], marks[-n][-(n-1)], marks[-n][-1]),
         sprintf(mask[2], marks[n-1], marks[n])
  )
  
  # Finding intervals
  x <- findInterval(x, marks, rightmost.closed = TRUE)
  factor(x, levels = 1:length(l), labels = l)
  
}