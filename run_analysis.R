rm(list = ls(all.names = TRUE))
graphics.off()
library(popbio)
library(magrittr)

## Libraries
library(eolpop)

## Inputs
nsim = 100

pop_size_mean = 350
pop_size_se = 0
pop_size_type = "Npair"

carrying_capacity_mean = 1000
carrying_capacity_se = 100


#(4.8/100)*sum(N000[-1])
#(0.7/100)*sum(N000[-1])
fatalities_mean = c(0, 3) #c(0, 5, 3, 4, 2, 1, 4, 2, 2, 3)
fatalities_se = c(0, 0.582) # c(0, rep(0.5,9))
length(fatalities_mean)

#survivals <- c(0.65, 0.75, 0.85, 0.94)
#fecundities <- c(0, 0, 0.05, 0.40)
#survivals <- c(0.47, 0.67, 0.67)
#fecundities <- c(0, 0.30, 1.16)
#survivals <- c(0.25, 0.30)
#fecundities <- c(0, 19.8)

survivals <- c(0.3, 0.65)
fecundities <- c(0, 4.5)

pop_growth_mean = 0.94
# lambda( build_Leslie(s = survivals, f = fecundities) )
pop_growth_se = 0


model_demo = NULL # M2_noDD_WithDemoStoch #M1_noDD_noDemoStoch #M4_WithDD_WithDemoStoch #M3_WithDD_noDemoStoch #
time_horizon = 30
coeff_var_environ = 0
fatal_constant = "h"


#if(length(fatalities_mean) > 2) cumulated_impacts = TRUE else cumulated_impacts = FALSE
cumulated_impacts = FALSE

onset_year = c(2010, 2013, 2016, 2016, 2017, 2019, 2020, 2020, 2020, 2021) #rep(2010, 10)#
length(onset_year)
onset_time = onset_year - min(onset_year) + 1
onset_time = c(min(onset_time), onset_time)
if(!cumulated_impacts) onset_time = NULL
onset_time

# Pop size total
N000 <- pop_vector(pop_size = pop_size_mean, pop_size_type = pop_size_type, s = survivals, f = fecundities)
sum(N000)

# Define K
K = pop_vector(pop_size = carrying_capacity_mean, pop_size_type = pop_size_type, s = survivals, f = fecundities) %>% sum
K

# Define theoretical rMAX for the species
rMAX_species <- rMAX_spp(surv = tail(survivals,1), afr = min(which(fecundities != 0)))
rMAX_species

# Define the (theoretical) theta parameter (shape of Density-dependence) for the species
# theta_spp(rMAX_species)
theta = 1

##
rMAX_use <- infer_rMAX(K = K, theta = theta,
                       pop_size_current = sum(N000), pop_growth_current = pop_growth_mean,
                       rMAX_theoretical = rMAX_species)
rMAX_use
rMAX_species



##  Avoid unrealistic scenarios
pop_growth_mean <- min(1 + rMAX_species, pop_growth_mean)
pop_growth_mean

lambda( build_Leslie(s = survivals, f = fecundities) )


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
names(time)

names(run0)
N <- run0$N ; dim(N)
#plot_traj(N, xlab = "Annee", ylab = "Taille de population (totale)")


dim(N)
dim(colSums(N))
colSums(N) %>% apply(., c(1,2), mean)

out = list()
out$run = run0

dim(out$run$N)

get_metrics(N = out$run$N)$scenario$impact[time_horizon, ,-1] %>% round(.,2)

res = get_metrics(N = out$run$N, cumulated_impacts = cumulated_impacts)





###

plot_impact(N, show_CI = 0.999, Legend = paste("sc", 1:length(fatalities_mean)))




##
# Pop size total
N00 <- pop_vector(pop_size = pop_size_mean, pop_size_type = pop_size_type, s = s_calibrated, f = f_calibrated)
sum(N00)

pop_size_mean
pop_size_type
sum(N00)
N00
sum(N000)

NN <- apply(N, c(1:3), mean)
colSums(NN[,1,1:2])
sum(NN[-c(1:2),1,1])/2
sum(NN[-1,1,1])
sum(NN[,1,1])


x11()
plot_traj(N, age_class_use = "pairs", fecundities = fecundities,
          Legend = paste("sc", 1:length(fatalities_mean)), ylim = c(0, NA))


plot_traj(N, age_class_use = "NotJuv0", fecundities = fecundities,
          Legend = paste("sc", 1:length(fatalities_mean)), ylim = c(0, NA))

plot_traj(N, age_class_use = "all", fecundities = fecundities,
          Legend = paste("sc", 1:length(fatalities_mean)), ylim = c(0, NA))


###
# plot_traj(N, Legend = paste("sc", 1:length(fatalities_mean)), ylim = c(0, NA))
