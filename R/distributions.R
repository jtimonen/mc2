#' Unnormalized log density of a donut disribution
#'
#' @export
#' @param x a point
#' @return unnormalized log density at \code{x}
donut <- function(x) {
  x1 <- x[,1]
  x2 <- x[,2]
  r  <- sqrt(x1^2 + x2^2)
  logp <- -(r-2)^2
  return(logp)
}

