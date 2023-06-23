################################################################
#####   4. Min_Ext
################################################################

# Function to calculate position of minimum value in each row
scalogram.calculate_min_extent <- function(row,extent) {
  min_value <- min(row)
  min_index <- which(row == min_value)
  result <- extent[min(min_index)]
  return(result)
}
