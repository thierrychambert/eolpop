##==============================================================================
##     Function to calibrate vital rates to match the desired lambda          ==
##==============================================================================

#' Calibration of vital rate values
#' Function to adjust the vital rates values in order to match the desired population growth rate
#'
#' @param inits intial values of survival and fecundities for the optimization
#' @param f a vector of survival probabilities for each age class
#' @param s a vector of fecundity values for each age clas
#' @param lam0 the desired population growth rate - the one to be matched
#'
#' @return a vector of adjusted values of survival and fecundities
#' @export
#'
#' @import popbio
#' @import magrittr
#' @rawNamespace import(stats, except = c(filter, lag))
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' calibrate_params(inits = NULL, s = s, f = f, lam0 = 1.08)
#'
calibrate_params <- function(inits = NULL, s, f, lam0){
  nac <- length(s)
  lam00 <- build_Leslie(s, f) %>% lambda
  fu <- f[f != 0]
  fo <- f[f == 0]

  ## Utility function to optimize
  uti_fun <- function(pars, fo, nac, lam0){
    s <- tail(pars, nac)
    fu <- head(pars, -nac)
    lam00 <- build_Leslie(s, f=c(fo, fu)) %>% lambda
    return(abs(lam0-lam00))
  }
  # End utility function

  # Set parameter boundaries for the optimization
  if(lam0 - lam00 < 0){
    #lower = c(rep(0, length(fu)), apply(cbind((s*0.5), 0.05), 1, max))
    lower = c(rep(0.01, length(fu)), s*0.10)
    upper = c(fu, s)
  }else{
    lower = c(fu, s)
    upper = c(rep(Inf, length(fu)), apply(cbind((s*1.25), 0.98), 1, min))
  }

  # Set initial values
  if(is.null(inits)) inits <- c(fu, s)

  # Optimize the utility function
  opt <- stats::optim(par = inits, fn = uti_fun, fo=fo, nac=nac, lam0=lam0,
                      lower = lower, upper = upper, method="L-BFGS-B")


  # Return output : New fecundity vector
  params <- c(tail(opt$par, nac), fo, head(opt$par, -nac))
  names(params) <- c(paste0("s", (1:nac)-1), paste0("f", (1:nac)-1))

  return(params)

} # End function
################################################################################




##==============================================================================
## Function to choose initial values for the calibration of vital rates       ==
##==============================================================================
#' Set initial values for the calibration of vital rates.
#' This function can be used  to speed up the optim process of the 'calibrate_params' function.
#'
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age clas
#' @param lam0 the desired population growth rate - the one to be matched
#'
#' @return a vetor of initial values for the calibration of survival and fecundity values
#' @export
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' init_calib(s = s, f = f, lam0 = 1.08)
#'
init_calib  <- function(s, f, lam0){

  A00 <- build_Leslie(s=s, f=f)

  # Difference to apply on lambda
  diff_rel_lam <- (lam0 - lambda(A00))/lambda(A00)
  d <- match_lam_delta(diff_rel_lam = diff_rel_lam, s=s, f=f)

  el <- elements_Leslie(s=s, f=f)
  vr0 = el$vital_rates
  vr1 = vr0*(1+d)

  nac = length(s)

  # Define s_init and f_init
  s_init <- head(vr1, nac)
  f_init <- tail(vr1, nac)

  # Apply correction to respect relative order of survivals and fecundities
  s_init <- apply(cbind(s_init, s_init * (s/s[1]) / (s_init/s_init[1])), 1 , max, na.rm = TRUE)
  f_init <- f_init * (f/max(f[f!=0])) / (f_init/max(f_init[f_init!=0]))
  f_init[is.nan(f_init)] <- 0

  # Calibrate survivals
  s_init <- (s_init %>% sapply(., max, 0.05)) %>% sapply(., min, 0.97)

  # Calibrate fecundities
  f_init <- f_init %>% sapply(., max, 0.001)
  f_init[f == 0] <- 0

  # Combine vital rates
  inits_vr <- c(s_init,f_init)
  inits_vr <- c(tail(inits_vr, nac), head(inits_vr, nac))
  inits <- inits_vr[inits_vr != 0]
  inits

  return(inits)
} # End function
################################################################################




##==============================================================================
## Function to infer the difference to apply to vital rates                   ==
##==============================================================================
#' Function to calculate the difference to apply to each vital rates to match the desired
#' population growth rate (lambda).
#' This function is used in the 'init_calib' function.
#'
#' @param diff_rel_lam a number. The relative difference of lambda (population growth rate)
#' to be matched
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age clas
#'
#' @return a vector of (absolute) difference to apply
#' @export
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#'
#' # Match for a 5% decrease in lambda
#' match_lam_delta(diff_rel_lam = -0.05, s, f)
match_lam_delta <- function(diff_rel_lam, s, f){

  nac = length(s)

  # Get elements of the Leslie matrix
  el <- elements_Leslie(s=s, f=f)
  A00 <- build_Leslie(s=s, f=f)

  # Extract sensitivities and elasticities (for each vital rate)
  S <- el$sens_elas$sensitivity
  E <- el$sens_elas$elasticity
  names(S) <- names(E) <- el$vr_names

  # Scale the DELTA for each vital rate based on sensitivities
  #scaling <- (sum(S)-S)
  scaling <- (max(E[E!=0])/E)
  scaling[el$vital_rates == 0] <- 0
  scaling

  # Infer the DELTA for each LESLIE MATRIX element
  d <- scaling * diff_rel_lam

  return(d)
} # End function
################################################################################
