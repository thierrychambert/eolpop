##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Function to project the population dynamic forward, over time             ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Population projection over time
#'
#' @param fatalities a vector (numeric). Each value correspond to the number of fatalities for each scenario.
#' The number of scenario assesed corresponds to the size of that vector.
#' @param intial_pop_vector a vector (numeric). Initial size of each age class. Typically, the output of the
#' pop_vector function.
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age class
#' @param model_demo an R object corresponding to the demographic model to be used. The 4 possible models currently are:
#' M1_noDD_noDemoStoch, M2_noDD_WithDemoStoch, M3_WithDD_noDemoStoch, M4_WithDD_WithDemoStoch,
#' @param time_horizon a number. The number of years (time horizon) over which to project the population dynamics.
#' @param coeff_var_environ a number. The coefficient of variation to model environment stochasticity.
#' @param fatal_constant text (character). Either "h" or "M". Using "h" sets the fatality RATE as the constant value across years.
#' Using "M" sets the NUMBER of fatalities as the constant value across years.
#' @param onset_time unused. Just here because it's required for cumulated impact and in higher level 'run_simul" function.
#' @param DD_params NULL or a list. Density-dependence parameters (rMAX, K, theta). Only used in DD models M3 and M4.
#'
#' @return a 3D array containing the size of each age class (dim 1), for each year (dim 2) and each scenario (dim 3).
#' @export
#'
#' @import magrittr
#'
#' @examples
#' s <- c(0.5, 0.7, 0.8, 0.95)
#' f <- c(0, 0, 0.05, 0.55)
#' N0 <- pop_vector(pop_size = 200, pop_size_type = "Npair", s, f)
#' pop_project(fatalities = c(0, 5, 10), intial_pop_vector = N0, s = s, f = f,
#' model_demo = M2_noDD_WithDemoStoch, time_horizon = 30,
#' coeff_var_environ = 0.1, fatal_constant = "h")
#'
pop_project <- function(fatalities,
                        intial_pop_vector,
                        s, f,
                        DD_params = NULL,
                        model_demo,
                        time_horizon,
                        coeff_var_environ,
                        fatal_constant = "h",
                        onset_time = NULL){


  M <- fatalities
  N0 <- intial_pop_vector
  cv_env <- coeff_var_environ

  # Number of years
  nyr <- time_horizon

  # Number of age classes
  nac <- length(s)

  # Number of fatalities scenario
  nsc <- length(fatalities)


  # Initiate Pop Size (output) Array
  N <- array(NA, dim = c(nac, nyr, nsc), dimnames = list(paste0("age", 1:nac),
                                                         paste0("year", 1:nyr),
                                                         paste0("scenario", 1:nsc)
  ))
  N[,1,] <-  N0

  ## Loops over time (years)
  for(t in 2:nyr){

    # Environmental Stochasticity
    ss = 1 - rnorm(nac, mean = qlogis(1-s), sd = cv_env/(s)) %>% plogis        ## sample thru the mortality rate : 1 - s
    ff = rnorm(nac, mean = log(f), sd = cv_env) %>% exp

    # Fatalities : constant number (M) or constant rate (h)
    if(fatal_constant == "M"){
      if(nac > 2){
        h <- sapply(M/apply(N[-1,t-1,], 2, sum), min, 1)
      }else{
        h <- sapply(M/N[-1,t-1,], min, 1)
      }
    } else {
      if(nac > 2){
        h <- sapply(M/apply(N[-1,1,], 2, sum), min, 1)
      }else{
        h <- sapply(M/N[-1,1,], min, 1)
      }
    }

    ## Prevent error, when N = 0 & M = 0
    if( any(is.na(h) | is.nan(h)) ) h[] <- 0

    ## Projection : apply the LESLIE matrix calculation forward
    # Scenario 0
    j=1
    run00 <- model_demo(N1 = N[,t-1,j], s = ss, f = ff, h = h[j], DD_params = DD_params,
                        use_ref_vr = FALSE, s_corr_factor = NULL, f_corr_factor = NULL)
    N[,t,j] <- run00$N2

    # Other scenarios
    if(nsc > 1){
      for(j in 2:nsc){
        run01 <- model_demo(N1 = N[,t-1,j], s = ss, f = ff, h = h[j], DD_params = DD_params,
                            use_ref_vr = TRUE, s_corr_factor = run00$s_corr_factor, f_corr_factor = run00$f_corr_factor)
        N[,t,j] <- run01$N2
      } # j
    } # if

  } # t

  return(N)

} # END FUNCTION
################################################################################
