scalogram.extract_by_extent <- function(raster_file, shapefile_file, x) {
  library(raster)
  library(sf)
  library(sp)
  #library(rgdal)
  #library(progress)
  
  # Load the shapefile
  shapefile <- st_read(shapefile_file)

    # Load the raster
  data <- raster(raster_file)
  
  # Check if raster has valid values
  if (!hasValues(data)) {
    stop("data raster does not have any valid values.")
  }
  
  ###If the coordinate system of raster and shapefile is different then this code will help in matching them
  # Check current coordinate systems
  shapefile_crs <- st_crs(shapefile)
  raster_crs <- crs(data)
  
  # If CRS doesn't match, reproject the shapefile to match the raster
  if (!identical(st_crs(shapefile), raster_crs)) {
    shapefile <- st_transform(shapefile, crs = raster_crs)
  }  
  
  

  # Vector of extents
  radii <- x  # Add more radii as needed
  
  
  #############################################################################################
  # Initialize an empty data frame to store the results
  result_df <- data.frame(ID = shapefile$id, Lat = shapefile$lat, Lon = shapefile$lon, stringsAsFactors = FALSE)
  # Extract the plot ID information
  ID <- shapefile$id
  
  # Loop over each radius
  for (i in seq_along(radii)) {
    radius <- radii[i]
    
    # define the plot edges based upon the plot radius
    yPlus <- shapefile$lat + (radius*res(data)[1])/2
    xPlus <- shapefile$lon + (radius*res(data)[1])/2
    yMinus <- shapefile$lat - (radius*res(data)[1])/2
    xMinus <- shapefile$lon - (radius*res(data)[1])/2
    
    # calculate polygon coordinates for each plot centroid
    square <- cbind(xMinus, yPlus,  # NW corner
                    xPlus, yPlus,  # NE corner
                    xPlus, yMinus,  # SE corner
                    xMinus, yMinus, # SW corner
                    xMinus, yPlus)  # NW corner again - close polygon
    
    # Extract the plot ID information
    ID <- shapefile$id
    
    # Create spatial polygons from coordinates
    polys <- SpatialPolygons(
      mapply(
        function(poly, id) {
          xy <- matrix(poly, ncol = 2, byrow = TRUE)
          Polygons(list(Polygon(xy)), ID = id)
        },
        split(square, row(square)),
        ID
      ),
      proj4string = crs(data)
    )
    
    # Extract the raster values for each polygon
    result <- extract(data, polys, fun = mean, na.rm = TRUE)
    
    # Add the result as a new column with a dynamic column name
    col_name <- paste("Extract_", radii[i],"x",radii[i], sep = "")
    #result_df[[col_name]] <- result
    result_df[[col_name]] <- as.vector(result)
  }
  # Return the results as a data frame
  return(data.frame(result_df))
}
  
