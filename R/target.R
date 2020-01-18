#' Unnormalized log density of a donut disribution
#'
#' @export
#' @param x a point
#' @return unnormalized log density at \code{x}
donut <- function(x) {
  sigma <- 0.3
  R <- 2
  if(is.null(dim(x))){
    x  <- t(as.matrix(x))
  }
  x1 <- x[,1]
  x2 <- x[,2]
  r  <- sqrt(x1^2 + x2^2)
  logp <- stats::dnorm(r, mean = R, sd = sigma, log = TRUE)
  return(logp)
}

#' Hamiltonian equations of the donut disribution
#'
#' @param location state vector of length 2
#' @param momentum momentum vector of length 2
#' @param mass mass
#' @return list
donut.hamiltonian <- function(location, momentum, mass) {
  sigma  <- 0.3
  R      <- 2
  r      <- sqrt(location[1]^2 + location[2]^2)
  d1     <- momentum/mass
  d2     <- 2*location/(sigma^2)*(R/r - 1)
  DT     <- list(location = d1, momentum = d2)
  return(DT)
}

