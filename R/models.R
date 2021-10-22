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
#' @param use_ref_vr Not used in this model, as there is no demographic stochasticity.
#' @param s_corr_factor Not used in this model, as there is no demographic stochasticity.
#' @param f_corr_factor Not used in this model, as there is no demographic stochasticity.
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
M1_noDD_noDemoStoch <- function(N1, s, f, h, DD_params = NULL,
                                use_ref_vr = FALSE, s_corr_factor = NULL, f_corr_factor = NULL){

  ## M1_noDD_noDemoStoch

  # Build the LESLIE matrix
  A <- build_Leslie(s = s*(1-h), f = f)

  # Apply the LESLIE matrix calculation at t+1
  N2 <- A%*%N1

  s_corr_factor <- f_corr_factor <- NULL
  return(list(N2 = N2, s_corr_factor = s_corr_factor, f_corr_factor = f_corr_factor))

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
#' @param use_ref_vr logical. If FALSE, classic demographic stichasticity is used in the model.
#' If TRUE, demographic stichasticity is not applied - instead it would be mimicked from the stochasticity of a reference run
#' (one must then provide s_corr_factor and f_corr_factor).
#' @param s_corr_factor Correction factor (on survivals) used to mimick demographic stochasticity.
#' @param f_corr_factor Correction factor (on fecundities) used to mimick demographic stochasticity.
#'
#' @return a vector of population sizes for each age class at time t2
#' @export
#'
#' @import popbio
#' @import magrittr
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' N1 <- c(50, 60, 75, 100)
#' h <- 0.05
#' M2_noDD_WithDemoStoch(N1, s, f, h)
#'
M2_noDD_WithDemoStoch <- function(N1, s, f, h, DD_params = NULL,
                                  use_ref_vr = FALSE, s_corr_factor = NULL, f_corr_factor = NULL){

  ## M2_noDD_WithDemoStoch

  # Number of age classes
  nac = length(s)

  # If this is a reference scenario : apply classic demographic stichasticity
  if(!use_ref_vr){

    # Survivors (to "natural mortality" (s) and Wind Turbine Fatalities (1-h))
    S <- rbinom(nac, N1, (1-h)*s)
    s_corr_factor <- S/((1-h)*s*N1)
    s_corr_factor[is.nan(s_corr_factor)] <- 0

    N2 <- c(rep(0, nac-1), tail(S,1)) + c(0, head(S,-1))

    # Births
    B <- rpois(nac, f*N2)
    f_corr_factor <- B/(f*N2)
    f_corr_factor[is.nan(f_corr_factor)] <- 0

    N2[1] <- sum(B)


    # Otherwise, use realized vital rates (s and f) values from the reference scenario
  }else{
    # Survivors using the "s_realized" from reference scenarios
    S <- N1*(1-h)*s*s_corr_factor

    # Active rounding
    S <- round(trunc(S) + rbinom(nac, size = 1, prob = S-trunc(S)))

    N2 <- c(rep(0, nac-1), tail(S,1)) + c(0, head(S,-1))

    # Births
    B <- sum(f*f_corr_factor*N2)

    # Active rounding
    B <- round(trunc(B) + rbinom(1, size = 1, prob = B-trunc(B)))

    N2[1] <- B
    s_corr_factor <- f_corr_factor <- NULL

  } # end if

  return(list(N2 = N2, s_corr_factor = s_corr_factor, f_corr_factor = f_corr_factor))

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
#' @param use_ref_vr Not used in this model, as there is no demographic stochasticity.
#' @param s_corr_factor Not used in this model, as there is no demographic stochasticity.
#' @param f_corr_factor Not used in this model, as there is no demographic stochasticity.
#'
#' @return a vector of population sizes for each age class at time t2
#' @export
#'
#' @import popbio
#' @import magrittr
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' N1 <- c(50, 60, 75, 100)
#' h <- 0.05
#' DD_params <- list(rMAX = 0.15, K = 1200, theta = 1)
#' M3_WithDD_noDemoStoch(N1, s, f, h,DD_params = DD_params)
#'
M3_WithDD_noDemoStoch <- function(N1, s, f, h, DD_params,
                                  use_ref_vr = FALSE, s_corr_factor = NULL, f_corr_factor = NULL){

  ## M3_WithDD_noDemoStoch

  # Extract DD parameters from list
  rMAX <- DD_params$rMAX
  K <- DD_params$K
  theta <- DD_params$theta

  # Apply density dependence effect
  lam_Nt <- 1 + rMAX*(1-(sum(N1)/K)^theta)

  # Calibrate vital rates to match lam_Nt
  A <- build_Leslie(s = s, f = f)
  diff_rel_lam <- (lam_Nt - lambda(A))/lambda(A)
  d <- match_lam_delta(diff_rel_lam = diff_rel_lam, s=s, f=f)
  #A01 <- A * (1+d)
  A01 <- A + d

  # Calibrate survivals
  keep <- which(A[-1,] != 0)
  s_Nt <- ((A01[-1,][keep]) %>% sapply(., max, 0.001)) %>% sapply(., min, 0.999)

  # Calibrate fecundities
  f00 <- (A01[1,]/s_Nt) %>% sapply(., max, 0.001)
  f_Nt <- c(0, head(f00,-1))
  f_Nt[f == 0] <- 0

  ## Check if approximation is close enough to desired lambda
 if( abs((lambda(build_Leslie(s = s_Nt, f = f_Nt)) - lam_Nt) / lam_Nt) > 0.05 ){

    #If difference is too large : Use optimisation function for better calibration
    inits <- c(f_Nt, s_Nt)
    inits <- inits[inits != 0]
    vr_calib <- calibrate_params(inits = inits, f = f_Nt, s = s_Nt, lam0 = lam_Nt)
    s_Nt <- head(vr_calib, length(s_Nt))
    f_Nt <- tail(vr_calib, length(f_Nt))

  } # if

  # Build the LESLIE matrix
  A_Nt <- build_Leslie(s = s_Nt*(1-h), f = f_Nt)

  # Apply the LESLIE matrix calculation at t+1
  N2 <- A_Nt%*%N1


  s_corr_factor <- f_corr_factor <- NULL
  return(list(N2 = N2, s_corr_factor = s_corr_factor, f_corr_factor = f_corr_factor))

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
#' @param use_ref_vr logical. If FALSE, classic demographic stichasticity is used in the model.
#' If TRUE, demographic stichasticity is not applied - instead it would be mimicked from the stochasticity of a reference run
#' (one must then provide s_corr_factor and f_corr_factor).
#' @param s_corr_factor Correction factor (on survivals) used to mimick demographic stochasticity.
#' @param f_corr_factor Correction factor (on fecundities) used to mimick demographic stochasticity.
#'
#' @import popbio
#' @import magrittr
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
M4_WithDD_WithDemoStoch <- function(N1, s, f, h, DD_params,
                                    use_ref_vr = FALSE, s_corr_factor = NULL, f_corr_factor = NULL){

  ## M4_WithDD_WithDemoStoch

  # Extract DD parameters from list
  rMAX <- DD_params$rMAX
  K <- DD_params$K
  theta <- DD_params$theta

  # Apply density dependence effect
  lam_Nt <- 1 + rMAX*(1-(sum(N1)/K)^theta)

  # Calibrate vital rates to match lam_Nt
  A <- build_Leslie(s = s, f = f)
  diff_rel_lam <- (lam_Nt - lambda(A))/lambda(A)
  d <- match_lam_delta(diff_rel_lam = diff_rel_lam, s=s, f=f)
  #A01 <- A * (1+d)
  A01 <- A + d

  # Calibrate survivals
  keep <- which(A[-1,] != 0)
  s_Nt <- ((A01[-1,][keep]) %>% sapply(., max, 0.001)) %>% sapply(., min, 0.999)

  # Calibrate fecundities
  f00 <- (A01[1,]/s_Nt) %>% sapply(., max, 0.001)
  f_Nt <- c(0, head(f00,-1))
  f_Nt[f == 0] <- 0

  ## Check if approximation is close enough to desired lambda
  if( abs((lambda(build_Leslie(s = s_Nt, f = f_Nt)) - lam_Nt) / lam_Nt) > 0.05 ){

    # If difference is too large : Use optimisation function for better calibration
    inits <- c(f_Nt, s_Nt)
    inits <- inits[inits != 0]
    vr_calib <- calibrate_params(inits = inits, f = f_Nt, s = s_Nt, lam0 = lam_Nt)
    s_Nt <- head(vr_calib, length(s_Nt))
    f_Nt <- tail(vr_calib, length(f_Nt))

  } # if


  # Number of age classes
  nac = length(s)


  # If this is a reference scenario : apply classic demographic stichasticity
  if(!use_ref_vr){

    # Survivors (to "natural mortality" (s) and Wind Turbine Fatalities (1-h))
    S <- rbinom(nac, N1, (1-h)*s_Nt)
    s_corr_factor <- S/((1-h)*s_Nt*N1)
    s_corr_factor[is.nan(s_corr_factor)] <- 0

    N2 <- c(rep(0, nac-1), tail(S,1)) + c(0, head(S,-1))

    # Births
    B <- rpois(nac, f_Nt*N2)
    f_corr_factor <- B/(f_Nt*N2)
    f_corr_factor[is.nan(f_corr_factor)] <- 0

    N2[1] <- sum(B)


  }else{

    # Survivors using the "correction factor" from reference scenarios
    S <- N1*(1-h)*s_Nt*s_corr_factor

    # Active rounding
    S <- round(trunc(S) + rbinom(nac, size = 1, prob = S-trunc(S)))

    N2 <- c(rep(0, nac-1), tail(S,1)) + c(0, head(S,-1))

    # Births
    B <- sum(f_Nt*f_corr_factor*N2)

    # Active rounding
    B <- round(trunc(B) + rbinom(1, size = 1, prob = B-trunc(B)))

    N2[1] <- B
    s_corr_factor <- f_corr_factor <- NULL
  }

  return(list(N2 = N2, s_corr_factor = s_corr_factor, f_corr_factor = f_corr_factor))
} # END FUNCTION
################################################################################
