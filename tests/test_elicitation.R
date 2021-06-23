## Elicited values for 4 experts
E1 <- c(50, 70, 100)
E2 <- c(200, 240, 280)
E3 <- c(100, 180, 300)
E4 <- c(120, 160, 220)
vals <- matrix(cbind(E1, E2, E3, E4), nrow = 3, byrow = FALSE)

# make small values
# vals <- vals/100

## Elicited coverage probabilities for the same 4 experts
Cp1 = 0.80
Cp2 = 0.90
Cp3 = 0.90
Cp4 = 0.70
Cp = c(Cp1, Cp2, Cp3, Cp4)

## Weights attributed to each expert
weights = c(1, 0.2, 0.2, 0.1)

## Lower and upper limits
# lower = 0
# upper = Inf

# Estimation
library(eolpop)
out <- elicitation(vals, Cp, weights = weights)


# Plot the curves
plot_elicitation(out)
#abline(v = out$mean_smooth, lty = 2, lwd = 3, col = "green")
#abline(v = out$mode_smooth, lty = 1, lwd = 3, col = "green")

## Results
# mean
out$mean_smooth

# mode
out$mode_smooth

# se
sqrt(out$var_smooth)

# 95% CI
qgamma(p = c(0.025, 0.975), shape = out$shape_smooth, rate = out$rate_smooth)


## Draw values from a smoothed, averaged distribution
sample_elicitation(n = 10, out = out)

## Draw values from an indiviual expert's distribution
sample_elicitation(n = 10, out = out, smooth = FALSE, expert = 3)
