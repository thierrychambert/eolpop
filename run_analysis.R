rm(list = ls(all.names = TRUE))
graphics.off()
library(popbio)
library(magrittr)

## Libraries
library(eolpop)

## Inputs
nsim = 5

pop_size_mean = 30000
pop_size_se = 0
pop_size_type = "Ntotal"

carrying_capacity_mean = 50000
carrying_capacity_se = 0

fatalities_mean = c(0, 1250) #c(0, 5, 3, 4, 2, 1, 4, 2, 2, 3)
fatalities_se = c(0, 0) # c(0, rep(0.5,9))
length(fatalities_mean)

# onset_year = c(2010, 2013, 2016) #, 2016, 2017, 2019, 2020, 2020, 2020, 2021) #rep(2010, 10)#

survivals <- c(0.70, 0.83, 0.88, 0.90)
fecundities <- c(0, 0, 0, 0.27)
lambda( build_Leslie(s = survivals, f = fecundities) )

#pop_growth_mean = 1.07932597
pop_growth_mean = 1.044
#pop_growth_mean = 1.10
pop_growth_se = 0

model_demo = NULL # M2_noDD_WithDemoStoch #M1_noDD_noDemoStoch #M4_WithDD_WithDemoStoch #M3_WithDD_noDemoStoch #
time_horizon = 30
coeff_var_environ = 0
fatal_constant = "M"
cumulated_impacts = FALSE

# Pop size total
N000 <- pop_vector(pop_size = pop_size_mean, pop_size_type = pop_size_type, s = survivals, f = fecundities)
sum(N000)

# Define K
K = pop_vector(pop_size = carrying_capacity_mean, pop_size_type = pop_size_type, s = survivals, f = fecundities) %>% sum
K

# Define theoretical rMAX for the species
#rMAX_species <- rMAX_spp(surv = tail(survivals,1), afr = min(which(fecundities != 0)))
rMAX_species <- 0.11
rMAX_species

##  Avoid unrealistic scenarios
# pop_growth_mean <- min(1 + rMAX_species, pop_growth_mean)
if(rMAX_species < (pop_growth_mean-1)) rMAX_species <- (pop_growth_mean-1)*1.1
rMAX_species

# Define the (theoretical) theta parameter (shape of Density-dependence) for the species
# theta_spp(rMAX_species)
theta = 1

##--------------------------------------------
##  Calibration                             --
##--------------------------------------------
# Calibrate vital rates to match the the desired lambda
inits <- init_calib(s = survivals, f = fecundities, lam0 = pop_growth_mean)
vr_calibrated <- calibrate_params(inits = inits, f = fecundities, s = survivals, lam0 = pop_growth_mean)
s_calibrated <- head(vr_calibrated, length(survivals))
f_calibrated <- tail(vr_calibrated, length(fecundities))

s_calibrated
f_calibrated
lambda( build_Leslie(s = s_calibrated, f = f_calibrated) )

##==============================================================================
##                         Analyses (simulations)                             ==
##==============================================================================
time <- system.time(
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

                    carrying_capacity_mean = carrying_capacity_mean,
                    carrying_capacity_se = carrying_capacity_se,

                    theta = theta,
                    rMAX_species = rMAX_species,

                    model_demo = NULL,
                    time_horizon = time_horizon,
                    coeff_var_environ = coeff_var_environ,
                    fatal_constant = fatal_constant)
)

#####################################################
time


out = list()
out$run = run0
dim(out$run$N)

res = get_metrics(N = out$run$N, cumulated_impacts = cumulated_impacts)
names(res)
res$scenario$Pext


plot_impact(N = out$run$N, sel_sc = "1", show_CI = 0.999, Legend = paste("sc", (1:length(fatalities_mean))-1))

x11()
plot_traj(N = out$run$N, age_class_use = "NotJuv0", fecundities = fecundities,
          Legend = paste("sc", 1:length(fatalities_mean)), ylim = c(0, NA))
