##==============================================================================
##               Function to extract Leslie matrix elements                   ==
##==============================================================================
#' Extract Leslie matrix elements
#'
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age class
#'
#' @return list, including : the Leslie matrix A, vital rates values (as a vector and as a list),
#' a symbolic expression of the Leslie matrix, the elements (expression) of the symbolic Leslie matrix,
#' a table of the sensitivities and elasticites ('sens_elas')
#' @export
#'
#' @import utils
#' @import popbio
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' elements_Leslie(s=s, f=f)
#'
elements_Leslie <- function(s, f){

  nac <- length(s)
  vr_list <- as.list(c(s,f))
  names(vr_list) <- c(paste0("s", (1:nac)-1), paste0("f", (1:nac)-1))
  vital_rates <- unlist(vr_list)

  # Intiate Leslie matrix : all 0
  A <- matrix(0, nrow = nac, ncol = nac)

  A[1,] <-  paste0(paste0("s", (1:nac)-1),"*",paste0("f", c((2:nac)-1,nac-1)))
  if(nac < 3){
    A[-1,] <- paste0("s", (1:nac)-1)
  }else{
    diag(A[-1,]) <- paste0("s", (1:(nac-1))-1)
    A[nac,nac] <- paste0("s", nac-1)
  }


  symbolic <- noquote(A)
  elements <- parse(text=t(A))

  A <- matrix( sapply(elements, eval, vr_list), nrow=nac, byrow=TRUE)

  sens_elas <- vitalsens(elements = elements, vitalrates = vr_list)

  return(list(A=A, vital_rates=vital_rates, symbolic=symbolic, elements=elements,
              vr_list = vr_list, vr_names = names(vr_list), sens_elas=sens_elas))
}
## End of function
################################################################################








##==============================================================================
##               Function to build the Leslie matrix                          ==
##==============================================================================

#' Build a Leslie matrix
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
build_Leslie <- function(s, f){ elements_Leslie(s=s, f=f)$A } # End of function
################################################################################
