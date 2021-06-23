library(popbio)
library(magrittr)
library(RColorBrewer)

##==============================================================================
##                               Fixed inputs                                 ==
##==============================================================================
# Nb simulations
nsim = 50

# Time Horizon
TH = 30

# Bird fatalities for sc0
M0 = 0
M0_se = 0

#M1 = 5
#M1_se = 1

# Population size
#N00_mu = 200
N00_se = 0

# Population trend
#lam0 = 1.01
lam0_se = 0

# Environmental stochasticity
cv_env = 0

# Number of age classes
nac = 4

# Survival of each age class
s_input <- c(0.5, 0.7, 0.8, 0.95)
# sm <- matrix(0, nrow=nac, ncol=1, dimnames = list(paste0("age", 1:nac), "Survival"))

# Fecundity of each age class
f_input <- c(0, 0, 0.05, 0.5)

# Model
 #M2_noDD_WithDemoStoch
model_demo = M1_noDD_noDemoStoch

## Build the Leslie matrix
#A <- build_Leslie(s = s, f = f)
#round(lambda(A), 2)
