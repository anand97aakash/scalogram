scalogram_extract_by_extent_bycsv <- function(raster_file, csv_file, x) {
  library(raster)
  library(sf)
  library(sp)
  #library(rgdal)
  #library(progress)
  
  # Load the CSV file
  data <- read.csv(csv_file)
  
  # Load the raster
  raster <- raster(raster_file)
  # Check if raster has valid values
  if (!hasValues(raster)) {
    stop("Raster does not have any valid values.")
  }
  
  # Vector of extents
  radii <- x
  
  #############################################################################################
  # Initialize an empty data frame to store the results
  result_df <- data.frame(ID = data$id, Lat = data$lat, Lon = data$lon, stringsAsFactors = FALSE)
  
  # Extract the plot ID information
  ID <- data$id
  
  # Loop over each radius
  for (i in seq_along(radii)) {
    radius <- radii[i]
    
    # define the plot edges based upon the plot radius
    yPlus <- data$lat + (radius * res(raster)[1])/2
    xPlus <- data$lon + (radius * res(raster)[1])/2
    yMinus <- data$lat - (radius * res(raster)[1])/2
    xMinus <- data$lon - (radius * res(raster)[1])/2
    
    # calculate polygon coordinates for each plot centroid
    square <- cbind(
      xMinus, yPlus,  # NW corner
      xPlus, yPlus,  # NE corner
      xPlus, yMinus,  # SE corner
      xMinus, yMinus, # SW corner
      xMinus, yPlus  # NW corner again - close polygon
    )
    
    # Create spatial polygons from coordinates
    polys <- SpatialPolygons(mapply(
      function(poly, id) {
        xy <- matrix(poly, ncol = 2, byrow = TRUE)
        Polygons(list(Polygon(xy)), ID = id)
      },
      split(square, row(square)),
      ID
    ),
    proj4string = crs(raster)
    )
    
    # Extract the raster values for each polygon
    result <- extract(raster, polys, fun = mean, na.rm = TRUE)
    
    # Add the result as a new column with a dynamic column name
    col_name <- paste("Extract_", radii[i], "x", radii[i], sep = "")
    result_df[[col_name]] <- as.vector(result)
  }
  return(data.frame(result_df))
}
