#' Hamiltonian Monte Carlo (HMC) sampling
#'
#' @inheritParams hmc.run
#' @return a list
hmc <- function(log_prob,
                hamiltonian,
                x0 = c(0,0),
                iter = 100,
                step_size = 0.1){

  if(!is.function(log_prob)){
    stop('log_prob must be a function!')
  }
  run <- hmc.run(log_prob, x0, iter)
  run <- postproc(run)
  return(run)

}


#' Run one HMC chain
#'
#' @param log_prob a function that takes input \code{theta} and returns
#' a value that is proportional to the log probability density at \code{theta}
#' @param hamiltonian a function that takes as input the location and momentum
#' and returns their time derivatives
#' @param x0 initial value
#' @param iter number of iterations
#' @param step_size leapfrog step size
#' @return a list
hmc.run <- function(log_prob, hamiltonian, x0, iter, step_size){
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
    stop("HMC not implemented!")
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
