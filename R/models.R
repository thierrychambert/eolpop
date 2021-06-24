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
#' @param DD_params density-dependence parameters. Not used in this model.
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
M1_noDD_noDemoStoch <- function(N1, s, f, h, DD_params = NULL){

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
#' @param DD_params density-dependence parameters. Not used in this model.
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
M2_noDD_WithDemoStoch <- function(N1, s, f, h, DD_params = NULL){

  # Number of age classes
  nac = length(s)

  # Survivors (to "natural mortality" (s) and Wind Turbine Fatalities (1-h))
  S <- rbinom(nac, N1, (1-h)*s)
  N2 <- c(rep(0, nac-1), tail(S,1)) + c(0, head(S,-1))

  # Births
  N2[1] <- sum(rpois(nac, f*N2))

  return(N2)

} # END FUNCTION
################################################################################




##==============================================================================
##                         ADD DENSITY DEPENDENCE                             ==
##==============================================================================




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Model 3: WITH Density-Dependence (DD), No Demographic Stochasticity       ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Demographic model with Density-Dependence, but without Demographic Stochasticity
#' In addition to natural survivals and fecundities, this demographic model includes
#' a specific harvest / fatality parameter.
#' NOTE : This is a POST-BREEDING demographic model.
#'
#' @param N1 a vector of population sizes for each age class at time t1
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age class
#' @param h a number. The harvest or fatality rate
#' @param DD_params a list containing the 3 parameters required to model density-dependence :
#' rMAX (maximum population intinsic rate of increase: lambda_max - 1),
#' K (carrying capacity), and
#' theta (shape of DD relationshp)
#'
#' @return a vector of population sizes for each age class at time t2
#' @export
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' N1 <- c(50, 60, 75, 100)
#' h <- 0.05
#' DD_params <- list(rMAX = 0.15, K = 1200, theta = 1)
#' M3_WithDD_noDemoStoch(N1, s, f, h,DD_params = DD_params)
#'
M3_WithDD_noDemoStoch <- function(N1, s, f, h, DD_params = NULL){

  # Extract DD parameters from list
  rMAX = DD_params$rMAX
  K = DD_params$K
  theta = DD_params$theta

  # Apply density dependence effect
  lam_Nt <- 1 + rMAX*(1-(sum(N1)/K)^theta)

  # Calibrate vital rates to match lam_Nt
  A <- build_Leslie(s = s, f = f)
  diff_rel_lam <- (lam_Nt - lambda(A))/lambda(A)
  d <- match_lam_delta(diff_rel_lam = diff_rel_lam, s=s, f=f)
  vr_Nt <- c(s,f) + d

  s_Nt <- head(vr_Nt, length(s)) %>% sapply(min, 0.999)
  f_Nt <- tail(vr_Nt, length(f))

  # Build the LESLIE matrix
  A_Nt <- build_Leslie(s = s_Nt, f = f_Nt)

  # Apply the LESLIE matrix calculation at t+1
  N2 <- A_Nt%*%N1*(1-h)

  return(N2)

} # END FUNCTION
################################################################################



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Model 4: With Density-Dependence (DD), WITH Demographic Stochasticity     ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Demographic model with Density-Dependence and INCLUDING Demographic Stochasticity
#' In addition to natural survivals and fecundities, this demographic model includes
#' a specific harvest / fatality parameter.
#' NOTE : This is a POST-BREEDING demographic model.
#'
#' @param N1 a vector of population sizes for each age class at time t1
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age class
#' @param h a number. The harvest or fatality rate
#' @param DD_params density-dependence parameters. Not used in this model.
#'
#' @return a vector of population sizes for each age class at time t2
#' @export
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' N1 <- c(50, 60, 75, 100)
#' h <- 0.05
#' DD_params <- list(rMAX = 0.15, K = 1200, theta = 1)
#' M4_WithDD_WithDemoStoch(N1, s, f, h, DD_params = DD_params)
#'
M4_WithDD_WithDemoStoch <- function(N1, s, f, h, DD_params){

  # Extract DD parameters from list
  rMAX = DD_params$rMAX
  K = DD_params$K
  theta = DD_params$theta

  # Apply density dependence effect
  lam_Nt <- 1 + rMAX*(1-(sum(N1)/K)^theta)

  # Calibrate vital rates to match lam_Nt
  A <- build_Leslie(s = s, f = f)
  diff_rel_lam <- (lam_Nt - lambda(A))/lambda(A)
  d <- match_lam_delta(diff_rel_lam = diff_rel_lam, s=s, f=f)
  vr_Nt <- c(s,f) + d

  s_Nt <- head(vr_Nt, length(s)) %>% sapply(min, 0.999)
  f_Nt <- tail(vr_Nt, length(f))

  # Number of age classes
  nac = length(s)

  # Survivors (to "natural mortality" (s) and Wind Turbine Fatalities (1-h))
  S <- rbinom(nac, N1, (1-h)*s_Nt)
  N2 <- c(rep(0, nac-1), tail(S,1)) + c(0, head(S,-1))

  # Births
  N2[1] <- sum(rpois(nac, f_Nt*N2))

  return(N2)

} # END FUNCTION
################################################################################
