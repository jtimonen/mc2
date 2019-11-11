#' Metropolis sampling
#'
#' @export
#' @param log_prob a function that takes input \code{x} and returns
#' a value that is proportional to the log probability density at \code{x}
#' @param x0 initial value
#' @param iter number of iterations
#' @param sigma_prop std of Gaussian proposal distribution
#' @return a numeric vector of length \code{iter}
metropolis <- function(log_prob,
                       x0 = c(0,0),
                       iter = 100,
                       sigma_prop = 1){

  if(!is.function(log_prob)){
    stop('log_prob must be a function!')
  }
  proposal <- function(x){
    x_prop <- stats::rnorm(n = length(x), mean = x, sd = sigma_prop)
    return(x_prop)
  }
  out <- run.chain.metropolis(log_prob, x0, iter, proposal)
  return(out)

}
