################################################################
#####  5. Max_Ext      Q. what if a row has multiple max values??
################################################################

# Function to calculate position of minimum value in each row
scalogram.calculate_max_extent <- function(row,extent) {
  max_value <- max(row)
  max_index <- which(row == max_value)
  result <- extent[max(max_index)]
  return(result)
}
