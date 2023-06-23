##################################################################
###### 1. Intercept
##################################################################

# Function to fit polynomial and calculate intercept
scalogram.fit_polynomial <- function(row, extent, degree) {
  x <- extent
  fit <- lm(row ~ poly(x, degree, raw = TRUE))
  coef(fit)[1]  # Intercept coefficient
}

