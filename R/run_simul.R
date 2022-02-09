##==============================================================================
##                     Function to run simulations                            ==
##==============================================================================
#' Run multiple population projections (simulations)
#'
#' @param nsim number of simulation
#' @param cumulated_impacts Logical. If TRUE, we used the projection model for cumulated impacts.
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
#'
#' @param carrying_capacity_mean a strictly positive number.
#' Average carrying capacity (= maximum size that the population can reach). Here, the unit is the same as pop_size_type.
#' It can thus be expressed as the total population or the number of pair.
#' @param carrying_capacity_se Standard Error for the carrying capacity.
#'
#' @param theta a strictly positive number. Parameter defining the shape of the density-dependence relationship.
#' The relationship is defined as : r <- rMAX*(1-(N/K)^theta)
#' Note lambda = r + 1
#'
#' @param rMAX_species the maximum value of rMAX for the species under consideration,
#' usually calculated using the Niel & Lebreton (2005) equation.
#' It can be calculated using the function rMAX_spp. See ?rMAX_spp for details.
#'
#' References :
#' Niel, C., and J. Lebreton. 2005. Using demographic invariants to detect overharvested bird
#' populations from incomplete data. Conservation Biology 19:826-835.
#'
#' @param model_demo is NULL, by default, because the model choice will be made inside each iteration (simulation),
#' base on the values of N0 and lam0 that are drawn.
#' But it can be forced by setting the value, which must then be an R object corresponding to the demographic model to be used.
#' The 4 possible models currently are: M1_noDD_noDemoStoch, M2_noDD_WithDemoStoch, M3_WithDD_noDemoStoch, M4_WithDD_WithDemoStoch,
#' @param time_horizon a number. The number of years (time horizon) over which to project the population dynamics.
#' @param coeff_var_environ a number. The coefficient of variation to model environment stochasticity.
#' @param fatal_constant text (character). Either "h" or "M". Using "h" sets the fatality RATE as the constant value across years.
#' Using "M" sets the NUMBER of fatalities as the constant value across years.
#'
#' @return a 4D array containing the size of each age class (dim 1), for each year (dim 2), each scenario (dim 3), and
#' each simulation iteration (dim 4)
#' @export
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
#'
#' time_horizon = 30
#' coeff_var_environ = 0.10
#' fatal_constant = "h"
#'
#' carrying_capacity_mean = 1200
#' carrying_capacity_se = 100
#' theta = 1
#' rMAX_species <- 0.15
#'
#' run_simul(nsim = 10, cumulated_impacts = FALSE,
#'            fatalities_mean, fatalities_se, onset_time = NULL,
#'            pop_size_mean, pop_size_se, pop_size_type,
#'            pop_growth_mean, pop_growth_se,
#'            survivals, fecundities,
#'            carrying_capacity_mean, carrying_capacity_se,
#'            theta,
#'            rMAX_species,
#'            model_demo = NULL, time_horizon, coeff_var_environ, fatal_constant)
#'
#'
run_simul <- function(nsim, cumulated_impacts,
                      fatalities_mean, fatalities_se, onset_time,
                      pop_size_mean, pop_size_se, pop_size_type,
                      pop_growth_mean, pop_growth_se,
                      survivals, fecundities,
                      carrying_capacity_mean, carrying_capacity_se,
                      theta = 1, rMAX_species,
                      model_demo = NULL, time_horizon, coeff_var_environ, fatal_constant){


  # Create object to store DD parameters
  DD_params <- list()

  # Fill the list of DD parameters
  DD_params$K <- NULL
  DD_params$theta <- theta
  DD_params$rMAX <- rMAX_species

  # Number of years
  nyr <- time_horizon

  # Number of age classes
  nac <- length(survivals)

  # Number of fatalities scenario (+1 because we include a base scenario of NO fatality)
  nsc <- length(fatalities_mean)

  # Initiate Pop Size (output) Array
  N <- array(NA, dim = c(nac, nyr, nsc, nsim), dimnames = list(paste0("age", 1:nac),
                                                               paste0("year", 1:nyr),
                                                               paste0("sc", (1:nsc)-1)
  ))
  # Object to store values of population growth drawn at each iteration
  lam_it <- rep(NA, nsim)

  # Time
  time_run <- system.time(

    ##--------------------------------------------
    # Start Loops over simulations              --
    ##--------------------------------------------
    for(sim in 1:nsim){

      ## PARAMETER UNCERTAINTY : draw values for each input
      # 1. Nomber of fatalities
      M <- NA
      for(j in 1:nsc){
        M[j] <- sample_gamma(1, mu = fatalities_mean[j], sd = fatalities_se[j])
      }

      # 2. Population size : draw and distribute by age class
      N0 <- sample_gamma(1, mu = pop_size_mean, sd = pop_size_se) %>%
        round %>%
        pop_vector(pop_size_type = pop_size_type, s = survivals, f = fecundities)

      # Draw carrying capacity
      carrying_capacity <- sample_gamma(1, mu = carrying_capacity_mean, sd = carrying_capacity_se) %>%
        round

      # Define K
      K <- sum(pop_vector(pop_size = carrying_capacity, pop_size_type = pop_size_type, s = survivals, f = fecundities))
      if(K < sum(N0)) K <- round(sum(N0)*1.05)
      DD_params$K <- K


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

model_demo = NULL

            # Choose the model demographique to use (if choice was not forced)
      if(is.null(model_demo)){

        ## Define the complete model by default
        model_demo <- M4_WithDD_WithDemoStoch

        # DECLINING (or stable), but initially LARGE population
        if(lam_it[sim] <= 1 & sum(N0) > 3000) model_demo <- M1_noDD_noDemoStoch

        # DECLINING  (or stable), and initially SMALL population
        if(lam_it[sim] <= 1 & sum(N0) <= 3000) model_demo <- M2_noDD_WithDemoStoch


        # GROWING population...
        if(lam_it[sim] > 1){






          # Extract rMAX
          new_rMAX <- infer_rMAX(K = K, theta = theta,
                     pop_size_current = sum(N0), pop_growth_current = lam_it[sim],
                     rMAX_theoretical = Inf)

          if(new_rMAX <= rMAX_species){
            DD_params$rMAX <- new_rMAX
          }else{

            # Infer theta
            DD_params$rMAX <- rMAX_species
            DD_params$theta <- infer_theta(K = K,
                       pop_size_current = sum(N0), pop_growth_current = lam_it[sim],
                       rMAX = rMAX_species)

          }


print(DD_params)



          # ... and initially LARGE population
          if(sum(N0) > 500) model_demo <- M3_WithDD_noDemoStoch


          # ... but initially SMALL population
          if(sum(N0) <= 500) model_demo <- M4_WithDD_WithDemoStoch

        } # if lam > 1


      } # end if "is.null"


      #
      if(cumulated_impacts){
        fun_project <- pop_project_cumulated_impacts
      }else{
        fun_project <- pop_project
        onset_time = NULL
      } # end if

      # Project population trajectory
      N[,,,sim] <- fun_project(fatalities = M, onset_time = onset_time, intial_pop_vector = N0,
                               s = s, f = f, DD_params = DD_params,
                               model_demo = model_demo, time_horizon = time_horizon,
                               coeff_var_environ = coeff_var_environ, fatal_constant = fatal_constant)
    } # sim ##-----------------------------------------------------------------------------------------

  ) # system.time

  return(list(time_run = time_run, N = N, lambdas = lam_it))

} # End of function
################################################################################
