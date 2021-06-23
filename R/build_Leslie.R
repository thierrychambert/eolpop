##==============================================================================
##               Function to build the Leslie matrix                          ==
##==============================================================================

#' Build a Leslie matrix
#' Function to build the Leslie matrix from survival and fecundity values
#'
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age class
#'
#' @return a Leslie matrix
#' @export
#'
#' @import utils
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' build_Leslie(s=s, f=f)
#'
build_Leslie <- function(s, f){

  nac <- length(s)

  # Intiate Leslie matrix : all 0
  A <- matrix(0, nrow = nac, ncol = nac)

  # Survivals
  if(nac == 2) A[-1,-nac] <- head(s,-1) else diag(A[-1,-nac]) <- head(s,-1)
  A[nac,nac] <- tail(s,1)

  # Fecundities
  f_bis <- c(f[-1], tail(f,1))
  A[1,] <- s*f_bis

  return(A)
} # End of function
################################################################################
