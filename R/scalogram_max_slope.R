
################################################################
#####  8.2 Maximum slope of the original polynomial curve
################################################################

scalogram.max_slope <- function(row, extent, degree) {
  fitted_curve <- lm(row ~ poly(extent, degree, raw = TRUE))    # x here is the area of extents
  deriv_coef<-function(x) {
    x <- coef(x)
    stopifnot(names(x)[1]=="(Intercept)")
    y <- x[-1]
    stopifnot(all(grepl("^poly", names(y))))
    px <- as.numeric(gsub("poly\\(.*\\)","",names(y)))
    rr <- setNames(c(y * px, 0), names(x))
    rr[is.na(rr)] <- 0
    rr
  }
  slope <- model.matrix(fitted_curve) %*% matrix(deriv_coef(fitted_curve), ncol=1)
  max_slope <- max(slope)
  #derivative <- diff(coef(fitted_curve)[1:(degree + 1)]) * factorial(degree)
  return(max_slope)
}
