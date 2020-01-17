#' Unnormalized log density of a donut disribution
#'
#' @export
#' @param x a point
#' @return unnormalized log density at \code{x}
donut <- function(x) {
  if(is.null(dim(x))){
    x  <- t(as.matrix(x))
  }
  x1 <- x[,1]
  x2 <- x[,2]
  r  <- sqrt(x1^2 + x2^2)
  logp <- -(r-2)^2
  return(logp)
}

#' Create contour plot of a density
#'
#' @export
#' @param fun function that evaluates (unnormalized) log density
#' @param grid grid bouds
#' @param N_grid number of grid points
#' @return plot of the density
plot_distribution <- function(fun = donut, grid = c(-4,-4,4,4), N_grid = 100){
  xx <- seq(grid[1], grid[3], length.out = N_grid)
  yy <- seq(grid[2], grid[4], length.out = N_grid)
  n1 <- length(xx)
  n2 <- length(yy)
  df <- matrix(0, n1*n2, 3)
  for(i in 1:n1){
    x <- xx[i]
    for(j in 1:n2){
      y <- yy[j]
      X <- t(as.matrix(c(x,y)))
      z <- fun(X)
      r <- (i-1)*n2 + j
      df[r,1] <- x
      df[r,2] <- y
      df[r,3] <- z
    }
  }
  df <- data.frame(df)
  colnames(df) <- c("x", "y", "z")
  df$z <- exp(df$z)
  df$z <- df$z/sum(df$z)
  p <- ggplot2::ggplot(df, ggplot2::aes(x=x,y=y)) + ggplot2::geom_raster(ggplot2::aes(fill=z)) +
    ggplot2::theme_minimal() + ggplot2::scale_fill_viridis_c()
  return(p)
}
