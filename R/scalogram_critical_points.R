


scalogram.critical_points <- function(data, extent, degree) {
  estimate_intersection <- function(row, extent, degree) {
    deriv <- scalogram.first_derivative(row, extent, 3)

    fitted_curve <- lm(deriv ~ poly(extent, degree = degree, raw = TRUE))
    coefficients <- coef(fitted_curve)

    poly_eq <- function(x) {
      y <- (as.double(coefficients[[1]])) + (as.double(coefficients[[2]])) * x^1 +
        (as.double(coefficients[[3]])) * x^2 + (as.double(coefficients[[4]]))
      return(y)
    }

    x <- seq(min(extent), max(extent), length.out = 100001)  # by = 1)
    y <- poly_eq(x)

    intersection <- x[which(diff(sign(y)) != 0)]
    return(intersection)
  }

  critical_point <- array(dim = c(nrow(data), 2))

  for (i in 1:nrow(data)) {
    row <- data[i, ]
    # Perform operations on the row
    # Access individual elements using row$column_name or row[["column_name"]]
    values <- estimate_intersection(row, extent, degree)

    # Check if either value is empty (numeric(0)) or contains NA
    if (length(values) == 0 || all(is.na(values))) {
      critical_point[i, ] <- c(NA, NA)
    } else if (length(values) == 1 || any(is.na(values))) {
      critical_point[i, ] <- c(values[1], NA)
    } else if (is.na(values[1]) || is.na(values[2])) {
      critical_point[i, ] <- c(NA, values[2])
    } else if (values[1] == values[2]) {
      critical_point[i, ] <- c(values[1], NA)
    } else {
      critical_point[i, ] <- values
    }
  }

  return(critical_point)
}


