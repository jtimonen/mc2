#' Run one metropolis chain
#'
#' @param log_prob a function that takes input \code{x} and returns
#' a value that is proportional to the log probability density at \code{x}
#' @param x0 initial value
#' @param iter number of iterations
#' @param draw_prop a function that takes input \code{x} and
#' draws a proposal \code{x_prop}
#' @return a \code{list(path, prop, lp, accept)}
run.chain.metropolis <- function(log_prob, x0, iter, draw_prop){
  d        <- length(x0)
  PATH     <- matrix(0, iter, d)
  PATH[1,] <- x0
  PROP     <- matrix(0, iter, d)
  PROP[1,] <- x0
  R        <- stats::runif(iter)
  LP       <- rep(0, iter)
  ACCEPT   <- rep(0, iter)
  LP[1]    <- log_prob(x0)

  for(i in 2:iter){
    x        <- PATH[i-1,]
    lp       <- LP[i-1]
    x_prop   <- draw_prop(x)
    PROP[i,] <- x_prop
    lp_prop  <- log_prob(x_prop)
    ratio    <- exp(lp_prop - lp)
    if(R[i]  < ratio){
      PATH[i,]  <- x_prop
      ACCEPT[i] <- 1
    }else{
      ACCEPT[i] <- 0
      PATH[i,]  <- x
    }
    LP[i] <- log_prob(PATH[i,])
  }
  ret <- list(x = PATH, x_prop = PROP, lp = LP, accept = ACCEPT)
  return(ret)
}
