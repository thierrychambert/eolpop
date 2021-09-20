##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Function to project the population dynamic forward, over time             ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Population projection over time
#'
#' @param fatalities a vector (numeric). Each value correspond to the number of fatalities for each scenario.
#' @param onset_time a vector (numeric). The times at which each wind farm fatality starts applying.
#' The number of scenario assesed corresponds to the size of that vector.
#' @param intial_pop_vector a vector (numeric). Initial size of each age class. Typically, the output of the
#' pop_vector function.
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age class
#' @param DD_params NULL or a list. Density-dependence parameters (rMAX, K, theta). Only used in DD models M3 and M4.
#' @param model_demo an R object corresponding to the demographic model to be used. The 4 possible models currently are:
#' M1_noDD_noDemoStoch, M2_noDD_WithDemoStoch, M3_WithDD_noDemoStoch, M4_WithDD_WithDemoStoch,
#' @param time_horzion a number. The number of years (time horizon) over which to project the population dynamics.
#' @param coeff_var_environ a number. The coefficient of variation to model environment stochasticity.
#' @param fatal_constant text (character). Either "h" or "M". Using "h" sets the fatality RATE as the constant value across years.
#' Using "M" sets the NUMBER of fatalities as the constant value across years.
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
#' model_demo = M2_noDD_WithDemoStoch, time_horzion = 30,
#' coeff_var_environ = 0.1, fatal_constant = "h")
#'
pop_project_cumulated_impacts <- function(fatalities,
                                          onset_time,
                                          intial_pop_vector,
                                          s, f,
                                          DD_params,
                                          model_demo,
                                          time_horzion,
                                          coeff_var_environ,
                                          fatal_constant = "h"){


  N0 <- intial_pop_vector
  cv_env <- coeff_var_environ

  # Number of years
  nyr <- time_horzion

  # Number of age classes
  nac <- length(s)

  # Number of fatalities scenario
  nsc <- length(fatalities)

  ## Fatalities taking onset time into account
  # Initiate matrix
  Mi <- matrix(fatalities, nrow = length(fatalities), ncol = nyr)

  # Fatalities from each wind farm
  for(j in 2:nrow(Mi)){
    if(onset_time[j] > 1) Mi[j,1:(onset_time[j]-1)] <- 0
  } # j

  # Cumulated Fatalities
  Mc <- Mi
  for(j in 2:nrow(Mc)) Mc[j,] <- apply(Mc[(j-1):j,], 2, sum)

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
      h <- sapply(Mc[,t-1]/apply(N[,t-1,], 2, sum), min, 1)
    } else {
      h <- sapply(Mc[,t-1]/apply(N[,1,], 2, sum), min, 1)
    }

    # Sample a seed for RNG
    seed <- ((((Sys.time() %>% as.numeric) %% 1e10) * 1e9) %% 1e5) %>% round

    ## Projection : apply the LESLIE matrix calculation forward
    # Scenario 0
    for(j in 1:nsc){
      set.seed(seed)
      N[,t,j] <- model_demo(N1 = N[,t-1,j], s = ss, f = ff, h = h[j], DD_params = DD_params)
    } # j

  } # t

  return(N)

} # END FUNCTION
################################################################################
