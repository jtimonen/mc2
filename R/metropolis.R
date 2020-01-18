#' Metropolis sampling
#'
#' @export
#' @inheritParams metropolis.run
#' @param sigma_prop std of Gaussian proposal distribution
#' @return a list
metropolis <- function(log_prob,
                       x0 = c(0,0),
                       iter = 100,
                       sigma_prop = 1){

  if(!is.function(log_prob)){
    stop('log_prob must be a function!')
  }
  draw.prop <- function(x){
    x_prop <- x + stats::rnorm(n = length(x), mean = 0, sd = sigma_prop)
    return(x_prop)
  }
  run <- metropolis.run(log_prob, x0, iter, draw.prop)
  run <- postproc(run)
  return(run)

}


#' Run one metropolis chain
#'
#' @param log_prob a function that takes input \code{x} and returns
#' a value that is proportional to the log probability density at \code{x}
#' @param x0 initial value
#' @param iter number of iterations
#' @param draw.prop a function that takes input \code{x} and
#' draws a proposal \code{x_prop}
#' @return a list
metropolis.run <- function(log_prob, x0, iter, draw.prop){
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
    x_prop   <- draw.prop(x) # draw proposal
    PROP[i,] <- x_prop
    lp_prop  <- log_prob(x_prop) # compute log prob of proposal
    ratio    <- exp(lp_prop - lp) # compute acceptance ratio
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
