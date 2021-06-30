rm(list = ls(all.names = TRUE))
graphics.off()



## Libraries
library(eolpop)

## Inputs
nsim = 1000

fatalities_mean = c(0, 2, 3, 5, 2)
fatalities_se = fatalities_mean*0.05

pop_size_mean = 200
pop_size_se = 30

pop_growth_mean = 0.98
pop_growth_se = 0

survivals <- c(0.5, 0.7, 0.8, 0.95)
fecundities <- c(0, 0, 0.05, 0.55)

model_demo = NULL # M2_noDD_WithDemoStoch #M1_noDD_noDemoStoch #M4_WithDD_WithDemoStoch #M3_WithDD_noDemoStoch #
time_horzion = 30
coeff_var_environ = 0.10
fatal_constant = "h"
pop_size_type = "Ntotal"

cumuated_impacts = TRUE

onset_year = 2000 + c(1, 3, 7, 15, 20)
onset_time = onset_year - min(onset_year) + 1

DD_params <- list(rMAX = 0.15, K = 1200, theta = 1)

##--------------------------------------------
##  Calibration : FYI, for table dsiply     --
##--------------------------------------------
# Calibrate vital rates to match the the desired lambda
inits <- init_calib(s = survivals, f = fecundities, lam0 = pop_growth_mean)
vr_calibrated <- calibrate_params(inits = inits, f = fecundities, s = survivals, lam0 = pop_growth_mean)
s_calibrated <- head(vr_calibrated, length(survivals))
f_calibrated <- tail(vr_calibrated, length(fecundities))

##==============================================================================
##                         Analyses (simulations)                             ==
##==============================================================================
run0 <- run_simul(nsim, cumuated_impacts,
                  fatalities_mean, fatalities_se, onset_time,
                  pop_size_mean, pop_size_se, pop_size_type,
                  pop_growth_mean, pop_growth_se,
                  survivals = s_calibrated, fecundities = f_calibrated,
                  DD_params = DD_params,
                  model_demo, time_horzion, coeff_var_environ, fatal_constant)


# save(run0, file = "./data/run0.rda")
names(run0)
run0$time_run
# saved time (ratio): 493/12

N <- run0$N ; dim(N)
out <- get_metrics(N, cumuated_impacts = cumuated_impacts)
names(out)

out$warning
names(out$scenario)

fatalities_mean
out$indiv_farm$impact[time_horzion,,]

out$scenario$impact[time_horzion,,]
out$scenario$Pext
out$scenario$DR_Pext

# plot_traj(N, xlab = "Annee", ylab = "Taille de population (totale)")

p <- plot_impact(N, onset_year = onset_year , xlab = "Annee", ylab = "Impact relatif")
p

#source("draws_histog.R")
#draws_histog(draws = run0$lambdas, mu = pop_growth_mean, se = pop_growth_se)
