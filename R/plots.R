#' Create contour plot of a density
#'
#' @export
#' @param fun function that evaluates (unnormalized) log density
#' @param grid grid bouds
#' @param N_grid number of grid points
#' @param color_low color scale lowest value color
#' @param color_high color scale highest value color
#' @param title plot title
#' @return a ggplot object
plt.surface <- function(fun = donut, grid_size = 3, N_grid = 200,
                        color_low = "gray15", color_high = "gray95",
                        title = NULL){
  if(length(grid_size)==1){
    grid <- c(-grid_size,-grid_size,grid_size,grid_size)
  }else{
    if(length(grid_size)==4){
      grid <- grid_size
    }else{
      stop("grid_size must have length 1 or 4!")
    }
  }
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
  colnames(df) <- c("x", "y", "prob")
  df$prob <- exp(df$prob)
  df$prob <- df$prob/sum(df$prob)
  p <- ggplot2::ggplot(df, ggplot2::aes(x=x,y=y)) +
    ggplot2::geom_raster(ggplot2::aes(fill=prob)) +
    ggplot2::theme_minimal() +
    scale_fill_gradient(low = color_low, high = color_high) +
    ggplot2::xlab('theta1') +
    ggplot2::ylab('theta2')
  if(!is.null(title)){
    p <- p + ggplot2::ggtitle(title)
  }
  p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank())
  return(p)
}


#' Add MCMC chain path to a contour plot of a density
#'
#' @export
#' @param p a ggplot object created using \code{plot.surface}
#' @param X a matrix with two columns
#' @param color line or point color
#' @param alpha line or point alpha
#' @param lines should the samples be connected with lines?
#' @return a ggplot object
add.path <- function(p, X, color = "firebrick3", alpha = 1.0, lines = TRUE){
  x <- X[,1]
  y <- X[,2]
  df_path <- data.frame(x,y)
  if(lines){
    p <- p + ggplot2::geom_path(data = df_path, mapping = ggplot2::aes(x = x, y = y),
                                color = color, alpha = alpha)
  }else{
    p <- p + ggplot2::geom_point(data = df_path, mapping = ggplot2::aes(x = x, y = y),
                                color = color, alpha = alpha)
  }
  return(p)
}

