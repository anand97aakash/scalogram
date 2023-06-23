scalogram.extract_by_extent <- function(raster_file, shapefile_file, x) {
  library(raster)
  library(sf)
  library(progress)

  # Load the shapefile
  shapefile <- st_read(shapefile_file)

  # Load the CumDHI raster
  CumDHI <- raster(raster_file)

  # Check if CumDHI has valid values
  if (!hasValues(CumDHI)) {
    stop("CumDHI raster does not have any valid values.")
  }

  # Create an empty list to store the results
  results_list <- list()

  # Create a progress bar
  pb <- progress_bar$new(total = length(x), width = 60)

  # Iterate over the values of x
  for (i in seq_along(x)) {
    nrow <- x[i]

    # Perform reduce neighborhood operation with mean reducer and specified kernel size
    L8_CumDHI_mean <- focal(CumDHI, w = matrix(1, nrow = nrow, ncol = nrow), fun = mean)

    # Extract the values to the shapefile
    L8_CumDHI_mean_extract <- extract(L8_CumDHI_mean, shapefile, fun = mean, na.rm = TRUE)

    # Store the results in the list
    results_list[[paste0("Extent_", nrow,"x",nrow)]] <- L8_CumDHI_mean_extract

    # Update the progress bar
    pb$tick()
  }

  # Return the results as a data frame
  return(data.frame(results_list))
}
