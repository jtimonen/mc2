#' Log density of the banana distiribution
#'
#' @export
#' @param x a point
#' @param r a shape parameter
#' @return log density at \code{x}
banana <- function(x, r = 0.05) {
  if(length(dim(x))==0){
    x <- t(as.matrix(x))
  }
  x1 <- x[,1]
  x2 <- x[,2]
  logp <- -x1^2/200 - 1/2*(x2 + r*x1^2 - 100*r)^2
  return(logp)
}
