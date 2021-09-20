rm(list = ls(all.names = TRUE))
graphics.off()


## Libraries
library(eolpop)

## Inputs
nsim = 10

fatalities_mean = c(0, 10, 5, 8)
fatalities_se = c(0, 0.05, 0.05, 0.05)

pop_size_mean = 200
pop_size_se = 25

pop_growth_mean = 1
pop_growth_se = 0

survivals <- c(0.5, 0.7, 0.8, 0.95)
fecundities <- c(0, 0, 0.05, 0.55)

model_demo = NULL # M2_noDD_WithDemoStoch #M1_noDD_noDemoStoch #M4_WithDD_WithDemoStoch #M3_WithDD_noDemoStoch #
time_horzion = 50
coeff_var_environ = 0.10
fatal_constant = "M"
pop_size_type = "Npair"

cumuated_impacts = TRUE

onset_year = c(2010, 2013, 2016)
onset_time = onset_year - min(onset_year) + 1
onset_time = c(min(onset_time), onset_time)

# Pop size total
sum(pop_vector(pop_size = pop_size_mean, pop_size_type = pop_size_type, s = survivals, f = fecundities))


# Define K
carrying_capacity = 500
theta = 1
K = pop_vector(pop_size = carrying_capacity, pop_size_type = pop_size_type, s = survivals, f = fecundities) %>% sum
K

# Define theoretical rMAX for the species
rMAX_species <- rMAX_spp(surv = tail(survivals,1), afr = min(which(fecundities != 0)))
rMAX_species

##  Avoid unrealistic scenarios
pop_growth_mean <- min(1 + rMAX_species, pop_growth_mean)
pop_growth_mean


##--------------------------------------------
##  Calibration                             --
##--------------------------------------------
# Calibrate vital rates to match the the desired lambda
inits <- init_calib(s = survivals, f = fecundities, lam0 = pop_growth_mean)
vr_calibrated <- calibrate_params(inits = inits, f = fecundities, s = survivals, lam0 = pop_growth_mean)
s_calibrated <- head(vr_calibrated, length(survivals))
f_calibrated <- tail(vr_calibrated, length(fecundities))

build_Leslie(s = s_calibrated, f = f_calibrated) %>% lambda

survivals = s_calibrated
fecundities = f_calibrated






#################################################################################################################

##--------------------------------------------
##  run simul                               --
##--------------------------------------------

# Create object to store DD parameters
DD_params <- list()

# Fill the list of DD parameters
DD_params$K <- NULL
DD_params$theta <- theta
DD_params$rMAX <- rMAX_species

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
sim=1


##########################

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


    lam0 > 1+rMAX_species
    pop_growth_mean


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
        DD_params$rMAX <- infer_rMAX(K = K, theta = theta,
                                     pop_size_current = sum(N0), pop_growth_current = lam_it[sim],
                                     rMAX_theoretical = rMAX_species)

        # ... and initially LARGE population
        if(sum(N0) > 500) model_demo <- M3_WithDD_noDemoStoch


        # ... but initially SMALL population
        if(sum(N0) <= 500) model_demo <- M4_WithDD_WithDemoStoch

      } # if lam > 1


    } # end if "is.null"




    fatalities = M
#######################################################################
    ##--------------------------------------------
    ##  Pop project cumulated                               --
    ##--------------------------------------------



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
    N <- array(0, dim = c(nac, nyr, nsc), dimnames = list(paste0("age", 1:nac),
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
        h <- Mc[,t-1]/apply(N[,t-1,], 2, sum)
      } else {
        h <- Mc[,t-1]/apply(N[,1,], 2, sum)
      }

      # Sample a seed for RNG
      seed <- runif(1, 0, 1e6)

      ## Projection : apply the LESLIE matrix calculation forward
      # Scenario 0
      for(j in 1:nsc){
        set.seed(seed)
        N[,t,j] <- model_demo(N1 = N[,t-1,j], s = ss, f = ff, h = h[j], DD_params = DD_params)
      } # j

    } # t






    t = 31
    j = 4



    ## M2_noDD_WithDemoStoch

    t = t+1

    h = sapply(Mc[,t-1]/apply(N[,t-1,], 2, sum), min, 1)
    h

    #N1 = N[,t,j]
    #N1 = N2
    N1 = c(2,1,1,3)

    # Number of age classes
    nac = length(s)

    # Survivors (to "natural mortality" (s) and Wind Turbine Fatalities (1-h))
    S <- rbinom(nac, N1, (1-h)*s)
    N2 <- c(rep(0, nac-1), tail(S,1)) + c(0, head(S,-1))

    # Births
    N2[1] <- sum(rpois(nac, f*N2))

    N2
    N[,t+1,j] = N2
