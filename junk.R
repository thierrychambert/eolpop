rm(list = ls(all.names = TRUE))
b1 = 5
mu = 12
b2 = 13
Cp = 0.99

vals <- matrix(c(b1, mu, b2))

out <- elicitation(vals, Cp = Cp)

plot_elicitation(out)
abline(v = c(b1,mu,b2))













vals[vals == 0] <- min(min(vals[vals != 0])/100, 0.0001)

# Get probability of quantiles
probs <- sapply(X = Cp, FUN = function(Cp) c((1-Cp)/2, 0.5, Cp+(1-Cp)/2))

# Fit
fit_raw <- SHELF::fitdist(vals = vals, probs = probs, lower = lower, upper = upper, weights = weights)

## Extract values (gamma)
shape_raw <- fit_raw$Gamma$shape
rate_raw <- fit_raw$Gamma$rate

## Smoothing
v_new <- seq(min(vals), max(vals), length.out = 100)
p_new <- SHELF::plinearpool(fit = fit_raw, x = v_new, d = "gamma", w = 1)
fit_smooth <- SHELF::fitdist(vals = v_new, probs = p_new, lower = lower, upper = upper, weights = 1)

v_new
p_new
