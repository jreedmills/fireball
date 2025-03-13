# Function to roll dice and return individual results
dice_roll <- function(n, d) {
  sample(1:d, size = n, replace = TRUE)
}