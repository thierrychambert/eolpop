## NOTE : Demographic models are POST-BREEDING

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Model 1: No Density-Dependence (noDD), No Demographic Stochasticity      ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Demographic model without Density-Dependence and without Demographic Stochasticity
#' In addition to natural survivals and fecundities, this demographic model includes
#' a specific harvest / fatality parameter.
#' NOTE : This is a POST-BREEDING demographic model.
#'
#' @param N1 a vector of population sizes for each age class at time t1
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age class
#' @param h a number. The harvest or fatality rate
#'
#' @return a vector of population sizes for each age class at time t2
#' @export
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' N1 <- c(50, 60, 75, 100)
#' h <- 0.05
#' M1_noDD_noDemoStoch(N1, s, f, h)
#'
M1_noDD_noDemoStoch <- function(N1, s, f, h){

  # Build the LESLIE matrix
  A <- build_Leslie(s = s, f = f)

  # Apply the LESLIE matrix calculation at t+1
  N2 <- A%*%N1*(1-h)

  return(N2)

} # END FUNCTION
################################################################################



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Model 2: No Density-Dependence (noDD), WITH Demographic Stochasticity    ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Demographic model without Density-Dependence, but INCLUDING Demographic Stochasticity
#' In addition to natural survivals and fecundities, this demographic model includes
#' a specific harvest / fatality parameter.
#' NOTE : This is a POST-BREEDING demographic model.
#'
#' @param N1 a vector of population sizes for each age class at time t1
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age class
#' @param h a number. The harvest or fatality rate
#'
#' @return a vector of population sizes for each age class at time t2
#' @export
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' N1 <- c(50, 60, 75, 100)
#' h <- 0.05
#' M2_noDD_WithDemoStoch(N1, s, f, h)
#'
M2_noDD_WithDemoStoch <- function(N1, s, f, h){

  nac = length(s)

  # Survivors (to "natural mortality" (s) and Wind Turbine Fatalities (1-h))
  S <- rbinom(nac, N1, (1-h)*s)
  N2 <- c(rep(0, nac-1), tail(S,1)) + c(0, head(S,-1))

  # Births
  N2[1] <- sum(rpois(nac, f*N2))

  return(N2)

} # END FUNCTION
################################################################################


