#' Run one metropolis chain
#'
#' @param log_prob a function that takes input \code{x} and returns
#' a value that is proportional to the log probability density at \code{x}
#' @param x0 initial value
#' @param iter number of iterations
#' @param proposal_dist a function that takes input \code{x} and
#' draws a proposal \code{x_prop}
#' @return a numeric vector of length \code{iter}
run.chain.metropolis <- function(log_prob, x0, iter, proposal_dist){
  return(x0)
}
