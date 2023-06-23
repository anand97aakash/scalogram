################################################################
#####  10. Intercept of 1st derivative
################################################################
# Function to calculate first derivative for each row using polynomial fitting
#scalogram.intercept <- function(row, degree) {
#  fitted_curve <- lm(row ~ poly(seq_along(row), degree, raw = TRUE))
#  derivative <- diff(coef(fitted_curve)[1:(degree + 1)]) * factorial(degree)
#  return(derivative[1])  ##[1] is the intercept
#}


scalogram.intercept <- function(row,extent, degree) {
  deriv <- scalogram.first_derivative(row, extent, degree)
  fitted_curve <- lm(deriv ~ poly(extent, degree = degree, raw = TRUE))
  coefficients <- coef(fitted_curve)
  poly_eq <- function(x) {
    y <- (as.double(coefficients[[1]])) + (as.double(coefficients[[2]])) * x^1 +
      (as.double(coefficients[[3]])) * x^2 + (as.double(coefficients[[4]]))
    return(y)
  }
  x1 <- seq(min(extent), max(extent), length.out = 100001)  # by = 1)
  y1 <- poly_eq(x1)
  intersection <- y1[which(diff(sign(x1)) != min(extent))]
  return(intersection[1])
}


