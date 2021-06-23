##==============================================================================
##   Function to build a vector of population size, for each age class        ==
##==============================================================================

#' Build a vector
#'
#' @param pop_size a single number. Total Population Size or Number of Pairs.
#' @param pop_size_type character value indicating if the provided value pop_size correpsonds to Total Population Size ("Ntotal")
#' or the Number of Pairs ("Npair"). A stable age distribution is used to infer the size of each age class.
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age class
#'
#' @import popbio
#'
#' @return a vector of sizes for each age class. The number of age class is deduced from the size of the 's' vector.
#' @export
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' pop_vector(pop_size = 200, pop_size_type = "Npair", s, f)
#'
pop_vector <- function(pop_size, pop_size_type = "Npair", s, f){

  N00 <- pop_size

  # Get the LESLIE matrix
  A <- build_Leslie(s, f)

  if(match.arg(arg = pop_size_type, choices = c("Npair","Ntotal")) == "Npair"){

    # If N0 is nb of pairs
    N0 <- round(stable.stage(A) * (N00*2)/tail(stable.stage(A),1))

  }else{

    # If N0 is total pop size (all age classes)
    if(match.arg(arg = pop_size_type, choices = c("Npair","Ntotal")) == "Ntotal"){

      N0 <- round(stable.stage(A) * N00)

      # Correct small difference due to rounding (not always needed)
      N0[1] <- N0[1] - (sum(N0) - N00)

    }  # end if 2

  } # end if / else 1

  return(N0)

} # End of function
################################################################################



