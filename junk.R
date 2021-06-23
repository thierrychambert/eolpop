rm(list = ls(all.names = TRUE))
library(eolpop)
library(magrittr)

s <- c(0.5, 0.7, 0.8, 0.95)
f <- c(0, 0, 0.05, 0.55)
N0 <- pop_vector(pop_size = 200, pop_size_type = "Npair", s, f)
intial_pop_vector <- N0

fatalities = c(0, 8, 10)
onset_time = c(1, 5, 20)

model_demo = M2_noDD_WithDemoStoch
time_horzion = 30
coeff_var_environ = 0.1
fatal_constant = "h"


TH = time_horzion

# Fatalities from each wind farm
Mi <- matrix(fatalities, nrow = length(fatalities), ncol = nyr)

# Fatalities from each wind farm
for(j in 2:nrow(Mi)){
  if(onset_time[j] > 1) Mi[j,1:(onset_time[j]-1)] <- 0
} # j

# Cumulated Fatalities
Mc <- Mi
for(j in 2:nrow(Mc)) Mc[j,] <- apply(Mc[(j-1):j,], 2, sum)

h <- Mc[,t-1]/apply(N[,t-1,], 2, sum)

h <- Mc[,t-1]/apply(N[,1,], 2, sum)




pop_project(fatalities = c(0, 5, 10), intial_pop_vector = N0, s = s, f = f,
 model_demo = M2_noDD_WithDemoStoch, time_horzion = 30,
 coeff_var_environ = 0.1, fatal_constant = "h")



j=2
start = onset_time[j]
vec = M[,j]


set_onset<- function(vec, start){
  if(start > 1) vec[1:(start-1)] <- 0
} # function
