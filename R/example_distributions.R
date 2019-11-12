#' Log density of the banana distiribution (2d)
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


#' Log density of a mixture of three Gaussians (2d)
#'
#' @export
#' @param x a point
#' @param m1 mean vector of component 1
#' @param m2 mean vector of component 2
#' @param m3 mean vector of component 3
#' @param S1 covariance matrix of component 1
#' @param S2 covariance matrix of component 2
#' @param S3 covariance matrix of component 3
#' @return log density at \code{x}
peaks <- function(x, m1 = c(-2,2), m2 = c(0,-2),  m3 = c(2,2),
                     S1 = diag(2), S2 = diag(2), S3 = diag(2)) {
  if(length(dim(x))==0){
    x <- t(as.matrix(x))
  }
  m1 <- mvtnorm::dmvnorm(x, mean = m1, sigma = S1, log = TRUE)
  m2 <- mvtnorm::dmvnorm(x, mean = m2, sigma = S2, log = TRUE)
  m3 <- mvtnorm::dmvnorm(x, mean = m3, sigma = S3, log = TRUE)
  logp <- log(exp(m1) + exp(m2) + exp(m3)) # not the best logsumexp
  return(logp)
}
