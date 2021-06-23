##==============================================================================
##                     Function to run simulations                            ==
##==============================================================================
#' Run multiple population projections (simulations)
#'
#' @param nsim number of simulation
#' @param cumuated_impacts Logical. If TRUE, we used the projection model for cumulated impacts.
#' @param fatalities_mean a vector (numeric). Average number of fatalities, for each scenario.
#' @param fatalities_se a vector (numeric). Standard Error for the number of fatalities, for each scenario (= uncertainties around the values provided).
#' @param onset_time a vector (numeric). The times at which each wind farm fatality starts applying.
#' @param pop_size_mean a single number. Average population size (either total population or number of pairs - see Ntype below).
#' @param pop_size_se Standard Error for population size (= uncertainty around the value provided).
#' @param pop_size_type character value indicating if the provided value pop_size correpsonds to Total Population Size ("Ntotal")
#' or the Number of Pairs ("Npair"). A stable age distribution is used to infer the size of each age class.
#' @param pop_growth_mean a number. Average population growth rate (lambda).
#' @param pop_growth_se Standard Error for population growth rate (= uncertainty around the value provided).
#' @param survivals a vector. Average survival probabilities for each age class.
#' @param fecundities a vector of fecundity values for each age class.
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
#' survivals <- c(0.5, 0.7, 0.8, 0.95)
#'
#' fecundities <- c(0, 0, 0.05, 0.55)
#'
#' model_demo = M2_noDD_WithDemoStoch
#'
#' time_horzion = 30
#' coeff_var_environ = 0.10
#' fatal_constant = "h"
#'
#' run_simul(nsim = 10, cumuated_impacts = FALSE,
#'            fatalities_mean, fatalities_se, onset_time = NULL,
#'            pop_size_mean, pop_size_se, pop_size_type,
#'            pop_growth_mean, pop_growth_se,
#'            survivals, fecundities,
#'            model_demo, time_horzion, coeff_var_environ, fatal_constant)
#'
#'
run_simul <- function(nsim, cumuated_impacts,
                      fatalities_mean, fatalities_se, onset_time,
                      pop_size_mean, pop_size_se, pop_size_type,
                      pop_growth_mean, pop_growth_se,
                      survivals, fecundities,
                      model_demo, time_horzion, coeff_var_environ, fatal_constant){

  # Coefficient of variation for environment stochasticity
  cv_env <- coeff_var_environ

  # Number of years
  nyr <- time_horzion

  # Number of age classes
  nac <- length(survivals)

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
        pop_vector(pop_size_type = pop_size_type, s = survivals, f = fecundities)

      if(pop_growth_se > 0){

        # 3. Population Growth Rate
        lam0 <- sample_gamma(1, mu = pop_growth_mean, sd = pop_growth_se)

        # 4. Calibrate vital rates to match the the desired lambda
        inits <- init_calib(s = survivals, f = fecundities, lam0 = lam0)

        vr <- calibrate_params(inits = inits, f = fecundities, s = survivals, lam0 = lam0)
        s <- head(vr, length(survivals))
        f <- tail(vr, length(fecundities))
        lam_it[sim] <- lambda(build_Leslie(s,f))

      }else{

        # No parameter uncertainty on population growth
        s <- survivals
        f <- fecundities
        lam_it[sim] <- lambda(build_Leslie(s,f))

      } # End if/else

      #
      if(cumuated_impacts){
        fun_project <- pop_project_cumulated_impacts
      }else{
        fun_project <- pop_project
        onset_time = NULL
      } # end if

      # Project population trajectory
      N[,,,sim] <- fun_project(fatalities = M, onset_time = onset_time, intial_pop_vector = N0, s = s, f = f,
                           model_demo = model_demo, time_horzion = time_horzion,
                           coeff_var_environ = coeff_var_environ, fatal_constant = fatal_constant)
    } # sim
  ) # system.time

  return(list(time_run = time_run, N = N, lambdas = lam_it))

} # End of function
################################################################################
