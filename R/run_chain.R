#' Run one metropolis chain
#'
#' @param log_prob a function that takes input \code{x} and returns
#' a value that is proportional to the log probability density at \code{x}
#' @param x0 initial value
#' @param iter number of iterations
#' @param proposal_dist a function that takes input \code{x} and
#' draws a proposal \code{x_prop}
#' @return a matrix of size \code{iter} x \code{length(x0)}
run.chain.metropolis <- function(log_prob, x0, iter, proposal_dist){
  d        <- length(x0)
  PATH     <- matrix(0, iter, d)
  PATH[1,] <- x0
  R        <- stats::runif(iter)
  LP       <- rep(0, iter)
  LP[1]    <- log_prob(x0)

  for(i in 2:iter){
    x       <- PATH[i-1,]
    lp      <- LP[i-1]
    x_prop  <- proposal_dist(x)
    lp_prop <- log_prob(x_prop)
    ratio   <- exp(lp_prop - lp)
    if(R[i] < ratio){
      PATH[i,] <- x_prop
    }else{
      PATH[i,] <- x
    }
  }
  return(PATH)
}
