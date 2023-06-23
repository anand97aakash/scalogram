
# Function to plot variable values with polynomial trendline and show equation and Spearman coefficient
scalogram.plot_trendline <- function(variable, extent, degree) {
  # Create x-values for scatterplot
  x <- extent#seq_along(variable)

  # Fit the polynomial trendline
  poly_fit <- lm(variable ~ poly(x, degree, raw = TRUE))

  # Get the predicted values
  y_pred <- predict(poly_fit)

  # Calculate Spearman coefficient
  #spearman_coef <- cor(variable, x, method = "spearman")

  # Calculate R-squared
  rss <- sum(residuals(poly_fit)^2)
  tss <- sum((variable - mean(variable))^2)
  rsquared <- 1 - rss / tss


  # Create the scatterplot
  plot(x, variable, pch = 16, col = "blue", xlab = "Extent", ylab = "Value",
       main = "Polynomial Trendline")

  # Add polynomial trendline
  x_seq <- seq(min(x), max(x), length.out = 100)
  lines(x_seq, predict(poly_fit, newdata = data.frame(x = x_seq)), col = "red", lwd = 2)

  # Coefficients of the polynomial equation
  coefficients <- coef(poly_fit)

  # Generate the equation string dynamically
  equation <- "y ="
  for (i in 1:(degree + 1)) {
    equation <- paste0(equation, " +", round(coefficients[i], 20), "* x^", i - 1)
  }
  equation <- gsub("\\+\\-", "-", equation)  # Replace "+-" with "-"
  print(equation)
  # Calculate the position for displaying text in the middle of the plot
  x_middle <- mean(range(x))
  y_middle <- mean(range(variable))

  # Set the vertical offset for each text annotation
  equation_offset <- 0.5
  rsquared_offset <- 1
  # Add trendline equation and Spearman coefficient
  #text(mean(x), max(variable), paste("Equation:", equation), pos = 4)
  text(mean(x), max(variable)-((max(variable)-min(variable))/length(variable)), paste("R-squared:", round(rsquared, 4)), pos = 4)

}
