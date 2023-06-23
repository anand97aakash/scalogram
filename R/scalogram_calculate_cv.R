################################################################
#####  7. CV
################################################################

# Function to calculate coefficient of variation for each row
scalogram.calculate_cv <- function(row) {
  sd_val <- sd(row)
  mean_val <- mean(row)
  cv_ <- sd_val / mean_val
  return(cv_)
}
