#' Print MCMC chain info
#'
#' @export
#' @param chain a list
#' @return nothing
print.info <- function(chain){
  ar <- round(chain$arate$whole, digits = 3)
  cat('Acceptance rate:', ar, '\n')
}


#' Compute acceptance rate
#'
#' @param a binary vector with length \code{iter}
#' @return a list containing acceptance rate during the whole chain,
#' during the first half, and during the second half of the chain
acceptance_rate <- function(a){
  n   <- length(a)
  idx <- floor(n/2)
  a1  <- a[2:idx]
  a2  <- a[(idx+1):n]
  a12 <- a[2:n]
  ar  <- function(x){sum(x)/length(x)}
  out <- list(whole=ar(a12), first=ar(a1), second=ar(a2))
  return(out)
}
