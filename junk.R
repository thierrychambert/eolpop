rm(list = ls(all.names = TRUE))
graphics.off()

## Libraries
library(eolpop)
library(magrittr)
library(popbio)



nsim = 10
time_horzion = 60
pop_growth_mean = 1.6


## Inputs

fatalities_mean = c(0, 5)
fatalities_se = c(0,0.05)

pop_size_mean = 200
pop_size_se = 25

pop_growth_se = 0.03

survivals <- c(0.5, 0.7, 0.8, 0.95)
fecundities <- c(0, 0, 0.05, 0.55)

model_demo = NULL # M2_noDD_WithDemoStoch #M1_noDD_noDemoStoch #M4_WithDD_WithDemoStoch #M3_WithDD_noDemoStoch #
coeff_var_environ = 0.10
fatal_constant = "h"
pop_size_type = "Npair"

cumuated_impacts = FALSE

onset_year = c(2010, 2013, 2016)
onset_time = onset_year - min(onset_year) + 1

# Pop size total
sum(pop_vector(pop_size = pop_size_mean, pop_size_type = pop_size_type, s = survivals, f = fecundities))

# Define K
carrying_capacity = 210
theta = 1
K = pop_vector(pop_size = carrying_capacity, pop_size_type = pop_size_type, s = survivals, f = fecundities) %>% sum
K

# Define theoretical rMAX for the species
rMAX_species <- rMAX_spp(surv = tail(survivals,1), afr = min(which(fecundities != 0)))
rMAX_species




##--------------------------------------------
##  Avoid dumb scenarios                    --
##--------------------------------------------
pop_growth_mean <- min(1 + rMAX_species, pop_growth_mean)
pop_growth_mean






#pop_growth_mean = 1 + rMAX_species

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













lam0 <- 1.5

# 4. Calibrate vital rates to match the the desired lambda
inits <- init_calib(s = survivals, f = fecundities, lam0 = lam0)

vr <- calibrate_params(inits = inits, f = fecundities, s = survivals, lam0 = lam0)
s <- head(vr, length(survivals))
f <- tail(vr, length(fecundities))
lam_it[sim] <- lambda(build_Leslie(s,f))
lam_it[sim]
lambda(build_Leslie(s,f))



model_demo <- M3_WithDD_noDemoStoch
DD_params$K <- 2500
nyr = 60










#######################################################################
    ##--------------------------------------------
    ##  Pop project                               --
    ##--------------------------------------------

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
        h <- M/apply(N[,t-1,], 2, sum)
      } else {
        h <- M/apply(N[,1,], 2, sum)
      }

      # Sample a seed for RNG
      seed <- ((((Sys.time() %>% as.numeric) %% 1e5) * 1e5) %% 1e5) %>% round
      #runif(1, 0, 1e6)

      ## Projection : apply the LESLIE matrix calculation forward
      # Scenario 0
      for(j in 1:nsc){
        set.seed(seed)
        N[,t,j] <- model_demo(N1 = N[,t-1,j], s = ss, f = ff, h = h[j], DD_params = DD_params)
      } # j

    } # t



colSums(N[,,1])



DD_params





h=0
N1 = N0
sum(N1) * lambda(build_Leslie(s,f))
K


lam_Nt <- 1 + rMAX*(1-(sum(N1)/K)^theta)
sum(N1) * lam_Nt


A_Nt <- build_Leslie(s = s_Nt, f = f_Nt)
lambda(A_Nt)

N2 <- A_Nt%*%N1
sum(N2)

#######################################################################
##--------------------------------------------
##  Model                             --
##--------------------------------------------


N1 = N2


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


## Check if approximation is close enough to desired lambda
if( abs((lambda(build_Leslie(s = s_Nt, f = f_Nt)) - lam_Nt) / lam_Nt) > 0.005 ){

  # If difference is too large : Use optimisation function for better calibration
  inits <- c(tail(vr_Nt, length(f)), head(vr_Nt, length(s)) %>% sapply(min, 0.999))
  inits <- inits[inits != 0]
  vr_calib <- calibrate_params(inits = inits, f = f_Nt, s = s_Nt, lam0 = lam_Nt)
  s_Nt <- head(vr_calib, length(s_Nt))
  f_Nt <- tail(vr_calib, length(f_Nt))

} # if


# Number of age classes
nac = length(s)

# Survivors (to "natural mortality" (s) and Wind Turbine Fatalities (1-h))
S <- rbinom(nac, N1, (1-h)*s_Nt)
N2 <- c(rep(0, nac-1), tail(S,1)) + c(0, head(S,-1))

# Births
N2[1] <- sum(rpois(nac, f_Nt*N2))

sum(N2)




























N1 = N2

  ## M3_WithDD_noDemoStoch

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

  lambda(build_Leslie(s = s_Nt, f = f_Nt))

  ## Check if approximation is close enough to desired lambda
  if( abs((lambda(build_Leslie(s = s_Nt, f = f_Nt)) - lam_Nt) / lam_Nt) > 0.005 ){

    # If difference is too large : Use optimisation function for better calibration
    inits <- c(tail(vr_Nt, length(f)), head(vr_Nt, length(s)) %>% sapply(min, 0.999))
    inits <- inits[inits != 0]
    vr_calib <- calibrate_params(inits = inits, f = f_Nt, s = s_Nt, lam0 = lam_Nt)
    s_Nt <- head(vr_calib, length(s_Nt))
    f_Nt <- tail(vr_calib, length(f_Nt))

  } # if

  # Build the LESLIE matrix
  A_Nt <- build_Leslie(s = s_Nt, f = f_Nt)

  # Apply the LESLIE matrix calculation at t+1
  N2 <- A_Nt%*%N1*(1-h)

sum(N2)












    build_Leslie(s = survivals, f = fecundities) %>% lambda
    N0 <- sample_gamma(1, mu = pop_size_mean, sd =  pop_size_se) %>%
      round %>%
      pop_vector(pop_size_type = pop_size_type, s = survivals, f = fecundities)

    sum(N0)
    pop_size_mean + 3*pop_size_se
    carrying_capacity < (pop_size_mean + 3*pop_size_se)



    sum(N0)
    K


    lam0
    1+rMAX_species
    pop_growth_mean

    DD_params




##==============================================================================
##                         Analyses (simulations)                             ==
##==============================================================================
run0 <- run_simul(nsim, cumuated_impacts,
                  fatalities_mean, fatalities_se, onset_time,
                  pop_size_mean, pop_size_se, pop_size_type,
                  pop_growth_mean, pop_growth_se,
                  survivals = s_calibrated, fecundities = f_calibrated,
                  carrying_capacity = carrying_capacity, theta = theta,
                  rMAX_species = rMAX_species,
                  model_demo, time_horzion, coeff_var_environ, fatal_constant)



N = run0$N
#plot_impact(N = N, xlab = "year", ylab = "pop size")
plot_traj(N, xlab = "Annee", ylab = "Taille de population (totale)")
abline(h = K)




(N[,1,,1])
colSums(N[,,,]) %>% max
