rm(list = ls(all.names = TRUE))
graphics.off()

## Libraries
library(eolpop)

## Inputs
nsim = 100

fatalities_mean = c(0, 2, 5, 10, 15)
fatalities_se = fatalities_mean*0.05

pop_size_mean = 200
pop_size_se = 30

pop_growth_mean = 1
pop_growth_se = 0

survivals <- c(0.5, 0.7, 0.8, 0.95)
fecundities <- c(0, 0, 0.05, 0.55)

model_demo = M2_noDD_WithDemoStoch
time_horzion = 30
coeff_var_environ = 0
fatal_constant = "h"
pop_size_type = "Ntotal"


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
run0 <- run_simul(nsim,
                  fatalities_mean, fatalities_se,
                  pop_size_mean, pop_size_se, pop_size_type,
                  pop_growth_mean, pop_growth_se,
                  survivals, fecundities,
                  model_demo, time_horzion, coeff_var_environ, fatal_constant)



# save(run0, file = "./data/run0.rda")
names(run0)
run0$time_run
lambdas <- run0$lambdas
N <- run0$N
dim(N)
# N[,,,1]

N <- run0$N
out <- get_metrics(N)
out

out[time_horzion,,]
 out[,"avg","sc1"]

# draws_histog(draws = lambdas, mu = pop_growth_mean, se = pop_growth_se)

# plot_traj(N, xlab = "Annee", ylab = "Taille de population (totale)")

plot_impact(N, xlab = "Annee", ylab = "Taille de population (totale)")


