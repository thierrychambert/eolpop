rm(list = ls(all.names = TRUE))
graphics.off()

## Libraries
library(eolpop)

## Inputs
nsim = 100

fatalities_mean = c(0, 3)
fatalities_se = fatalities_mean*0.05

pop_size_mean = 200
pop_size_se = 30

pop_growth_mean = 1.1
pop_growth_se = 0.05

survivals <- c(0.5, 0.7, 0.8, 0.95)
fecundities <- c(0, 0, 0.05, 0.55)

model_demo = M3_WithDD_noDemoStoch #M2_noDD_WithDemoStoch #
time_horzion = 30
coeff_var_environ = 0.10
fatal_constant = "h"
pop_size_type = "Ntotal"

cumuated_impacts = FALSE
onset_time = c(1, 3, 7, 15)

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

N <- run0$N
out <- get_metrics(N)
out[time_horzion,"avg",]

# Impact par parc
# for(j in 2:length())
#j=4
#out[time_horzion, -2, j] - out[time_horzion, -2, j-1]


plot_traj(N, xlab = "Annee", ylab = "Taille de population (totale)")

plot_impact(N, xlab = "Annee", ylab = "Taille de population (totale)")


##==============================================================================
##                         Check lambda draws                                 ==
##==============================================================================
draws_histog <- function(draws, mu, se){

  # Plot histogram
  h <- hist(draws, breaks = length(draws)/10, border = 0)

  # Theoretical Normal Curve
  par(new=T)
  curve(dnorm(x, mean=mu, sd=se), add=FALSE, lwd=3, col="darkblue",
        xlim = c(min(draws), max(draws)), axes = FALSE, xlab = "", ylab = "")

} # End function
################################################################################


draws_histog(draws = run0$lambdas, mu = pop_growth_mean, se = pop_growth_se)
