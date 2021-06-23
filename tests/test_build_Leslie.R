# Survival of each age class
s <- c(0.5, 0.7, 0.8, 0.95)

# Fecundity of each age class
f <- c(0, 0, 0.05, 0.55)

# Build the Leslie matrix
library(eolpop)
build_Leslie(s=s, f=f)
