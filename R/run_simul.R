##==============================================================================
##                     Function to run simulations                            ==
##==============================================================================
#' Run multiple population projections (simulations)
#'
#' @param nsim number of simulation
#' @param fatalities_mean a vector (numeric). Average number of fatalities, for each scenario.
#' @param fatalities_se a vector (numeric). Standard Error for the number of fatalities, for each scenario (= uncertainties around the values provided).
#' @param pop_size_mean a single number. Average population size (either total population or number of pairs - see Ntype below).
#' @param pop_size_se Standard Error for population size (= uncertainty around the value provided).
#' @param pop_size_type character value indicating if the provided value pop_size correpsonds to Total Population Size ("Ntotal")
#' or the Number of Pairs ("Npair"). A stable age distribution is used to infer the size of each age class.
#' @param pop_growth_mean a number. Average population growth rate (lambda).
#' @param pop_growth_se Standard Error for population growth rate (= uncertainty around the value provided).
#' @param survivals_mean a vector. Average survival probabilities for each age class.
#' @param fecundities_mean a vector of fecundity values for each age class.
#' @param model_demo an R object corresponding to the demographic model to be used. The 4 possible models currently are:
#' M1_noDD_noDemoStoch, M2_noDD_WithDemoStoch, M3_WithDD_noDemoStoch, M4_WithDD_WithDemoStoch,
#' @param time_horzion a number. The number of years (time horizon) over which to project the population dynamics.
#' @param coeff_var_environ a number. The coefficient of variation to model environment stochasticity.
#' @param fatal_constant text (character). Either "h" or "M". Using "h" sets the fatality RATE as the constant value across years.
#' Using "M" sets the NUMBER of fatalities as the constant value across years.
#'
#' @return a 4D array containing the size of each age class (dim 1), for each year (dim 2), each scenario (dim 3), and
#' each simulation iteration (dim 4)
#' @export
#'
#' @import magrittr
#' @import popbio
#'
#' @examples
#' fatalities_mean = c(0, 5, 10, 15, 20)
#' fatalities_se = fatalities_mean*0.05
#'
#' pop_size_mean = 200
#' pop_size_se = 30
#' pop_size_type = "Npair"
#'
#' pop_growth_mean = 1
#' pop_growth_se = 0.03
#'
#' survivals_mean <- c(0.5, 0.7, 0.8, 0.95)
#'
#' fecundities_mean <- c(0, 0, 0.05, 0.55)
#'
#' model_demo = M2_noDD_WithDemoStoch
#'
#' time_horzion = 30
#' coeff_var_environ = 0.10
#' fatal_constant = "h"
#'
#' run_simul(nsim = 10,
#'            fatalities_mean, fatalities_se,
#'            pop_size_mean, pop_size_se, pop_size_type,
#'            pop_growth_mean, pop_growth_se,
#'            survivals_mean, fecundities_mean,
#'            model_demo, time_horzion, coeff_var_environ, fatal_constant)
#'
#'
run_simul <- function(nsim,
                      fatalities_mean, fatalities_se,
                      pop_size_mean, pop_size_se, pop_size_type,
                      pop_growth_mean, pop_growth_se,
                      survivals_mean, fecundities_mean,
                      model_demo, time_horzion, coeff_var_environ, fatal_constant){

  # Coefficient of variation for environment stochasticity
  cv_env <- coeff_var_environ

  # Number of years
  nyr <- time_horzion

  # Number of age classes
  nac <- length(survivals_mean)

  # Number of fatalities scenario (+1 because we include a base scenario of NO fatality)
  nsc <- length(fatalities_mean)

  # Initiate Pop Size (output) Array
  N <- array(NA, dim = c(nac, nyr, nsc, nsim), dimnames = list(paste0("age", 1:nac),
                                                               paste0("year", 1:nyr),
                                                               paste0("sc", (1:nsc)-1)
  ))
  # object to store values of population growth drawn at each iteration
  lam_it <- rep(NA, nsim)

  time_run <- system.time(
    for(sim in 1:nsim){

      ## PARAMETER UNCERTAINTY : draw values for each input
      # 1. Nomber of fatalities
      M <- NA
      for(j in 1:nsc){
        M[j] <- sample_gamma(1, mu = fatalities_mean[j], sd = fatalities_se[j])
      }

      # 2. Population size : draw and distribute by age class
      N0 <- sample_gamma(1, mu = pop_size_mean, sd =  pop_size_se) %>%
        round %>%
        pop_vector(pop_size_type = pop_size_type, s = survivals_mean, f = survivals_mean)

      if(pop_growth_se > 0){

        # 3. Population Growth Rate
        lam0 <- sample_gamma(1, mu = pop_growth_mean, sd = pop_growth_se)

        # 4. Calibrate vital rates to match the the desired lambda
        inits <- init_calib(s = survivals_mean, f = fecundities_mean, lam0 = lam0)

        vr <- calibrate_params(inits = inits, f = fecundities_mean, s = survivals_mean, lam0 = lam0)
        s <- head(vr, length(survivals_mean))
        f <- tail(vr, length(fecundities_mean))
        lam_it[sim] <- lambda(build_Leslie(s,f))

      }else{

        # No parameter uncertainty on population growth
        s <- survivals_mean
        f <- fecundities_mean
        lam_it[sim] <- lambda(build_Leslie(s,f))

      } # End if/else

      # Project population trajectory
      N[,,,sim] <- pop_project(fatalities = M, intial_pop_vector = N0, s = s, f = f,
                           model_demo = model_demo, time_horzion = time_horzion,
                           coeff_var_environ = coeff_var_environ, fatal_constant = fatal_constant)
    } # sim
  ) # system.time

  return(list(time_run = time_run, N = N, lambdas = lam_it))

} # End of function
################################################################################
