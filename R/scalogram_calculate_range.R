################################################################
#####  6. Range
################################################################

# Function to calculate minimum value in each row
scalogram.calculate_range <- function(row) {
  min_val <- min(row)
  max_val <- max(row)
  return(max_val - min_val)
}
