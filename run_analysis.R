rm(list = ls(all.names = TRUE))
graphics.off()
library(popbio)

## Libraries
library(eolpop)

## Inputs
nsim = 10

fatalities_mean = c(0, 10, 5, 8)
fatalities_se = c(0, 0.05, 0.05, 0.05)

pop_size_mean = 200
pop_size_se = 25

pop_growth_mean = 1.1
pop_growth_se = 0

survivals <- c(0.5, 0.7, 0.8, 0.95)
fecundities <- c(0, 0, 0.05, 0.55)

model_demo = NULL # M2_noDD_WithDemoStoch #M1_noDD_noDemoStoch #M4_WithDD_WithDemoStoch #M3_WithDD_noDemoStoch #
time_horzion = 50
coeff_var_environ = 0.10
fatal_constant = "h"
pop_size_type = "Ntotal"

cumulated_impacts = TRUE

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

lambda( build_Leslie(s = s_calibrated, f = f_calibrated) )



#pop_size_mean = 200
#pop_growth_mean = 1.1
#fatal_constant = "M"
#cumulated_impacts = FALSE

pop_size_type = "Ntotal"
carrying_capacity = 500


##==============================================================================
##                         Analyses (simulations)                             ==
##==============================================================================
run0 <- run_simul(nsim = nsim,
                            cumulated_impacts = cumulated_impacts,

                            fatalities_mean = fatalities_mean,
                            fatalities_se = fatalities_se,
                            onset_time = onset_time,

                            pop_size_mean = pop_size_mean,
                            pop_size_se = pop_size_se,
                            pop_size_type = pop_size_type,

                            pop_growth_mean = pop_growth_mean,
                            pop_growth_se = pop_growth_se,

                            survivals = s_calibrated,
                            fecundities = f_calibrated,

                            carrying_capacity = carrying_capacity,
                            theta = theta,
                            rMAX_species = rMAX_species,

                            model_demo = NULL,
                            time_horzion = time_horzion,
                            coeff_var_environ = coeff_var_environ,
                            fatal_constant = fatal_constant)


#####################################################


names(run0)
N <- run0$N ; dim(N)
plot_traj(N, xlab = "Annee", ylab = "Taille de population (totale)")
abline(h = K)
