rm(list = ls(all.names = TRUE))
graphics.off()
library(popbio)
library(magrittr)

## Libraries
library(eolpop)

## Inputs
nsim = 10

pop_size_mean = 50
pop_size_se = 0

carrying_capacity = 5000


#(4.8/100)*sum(N000[-1])
#(0.7/100)*sum(N000[-1])
fatalities_mean = c(0, 5, 3, 4, 2, 1, 4)
fatalities_se = c(0, rep(0.5,6))


survivals <- c(0.47, 0.67, 0.67)
fecundities <- c(0, 0.30, 1.16)

pop_growth_mean = 1.03
# lambda( build_Leslie(s = survivals, f = fecundities) )
pop_growth_se = 0.01


model_demo = NULL # M2_noDD_WithDemoStoch #M1_noDD_noDemoStoch #M4_WithDD_WithDemoStoch #M3_WithDD_noDemoStoch #
time_horzion = 30
coeff_var_environ = 0
fatal_constant = "h"
pop_size_type = "Npair"

#if(length(fatalities_mean) > 2) cumulated_impacts = TRUE else cumulated_impacts = FALSE
cumulated_impacts = TRUE

onset_year = c(2010, 2013, 2016, 2016, 2017, 2019, 2020)
onset_time = onset_year - min(onset_year) + 1
onset_time = c(min(onset_time), onset_time)
if(!cumulated_impacts) onset_time = NULL
onset_time

# Pop size total
N000 <- pop_vector(pop_size = pop_size_mean, pop_size_type = pop_size_type, s = survivals, f = fecundities)
sum(N000)

# Define K
theta = 1
K = pop_vector(pop_size = carrying_capacity, pop_size_type = pop_size_type, s = survivals, f = fecundities) %>% sum
K

# Define theoretical rMAX for the species
rMAX_species <- rMAX_spp(surv = tail(survivals,1), afr = min(which(fecundities != 0)))
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

lambda( build_Leslie(s = s_calibrated, f = f_calibrated) )

##==============================================================================
##                         Analyses (simulations)                             ==
##==============================================================================
system.time(
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
)

#####################################################


names(run0)
N <- run0$N ; dim(N)
#plot_traj(N, xlab = "Annee", ylab = "Taille de population (totale)")

dim(N)
dim(colSums(N))
colSums(N) %>% apply(., c(1,2), mean)

out = list()
out$run = run0

dim(out$run$N)

get_metrics(N = out$run$N)$scenario$impact[time_horzion, ,-1] %>% round(.,2)

res = get_metrics(N = out$run$N, cumulated_impacts = cumulated_impacts)

###

n_farm <- (dim(res$indiv_farm$impact)[3]-1)
fil <- paste0(round(t(res$indiv_farm$impact[time_horzion, -2, -1]),2)*100, "%")
matrix(fil,
       nrow = n_farm,
       dimnames = list(paste("Parc",1:n_farm), c("Impact", "IC (min)", "IC (max)"))
)

###

x11()
plot_impact(N)

###

n_scen <- (dim(res$scenario$impact)[3]-1)
fil <- paste0(round(t(res$scenario$impact[time_horzion, -2, -1]),2)*100, "%")
matrix(fil,
       nrow = n_scen,
       dimnames = list(NULL, c("Impact", "IC (min)", "IC (max)"))
)
