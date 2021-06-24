rm(list = ls(all.names = TRUE))
library(eolpop)
library(magrittr)
library(popbio)

s <- c(0.5, 0.7, 0.8, 0.95)
f <- c(0, 0, 0.05, 0.55)
N1 <- pop_vector(pop_size = 200, pop_size_type = "Npair", s, f)
h <- 0.05






rMAX = 0.15
K = sum(N1)*5
theta = 1

# Apply density dependence effect
N1 = K
lam_Nt <- 1 + rMAX*(1-(sum(N1)/K)^theta)
lam_Nt


# Calibrate vital rates to match lam_Nt
A <- build_Leslie(s = s, f = f)
diff_rel_lam <- (lam_Nt - lambda(A))/lambda(A)
d <- match_lam_delta(diff_rel_lam = diff_rel_lam, s=s, f=f)
vr_Nt <- c(s,f) + d

s_Nt <- head(vr_Nt, length(s))
f_Nt <- tail(vr_Nt, length(f))

A_Nt <- build_Leslie(s = s_Nt, f = f_Nt)
lambda(A_Nt)
















# Calibrate vital rates to match lam_Nt
inits <- init_calib(s = s, f = f, lam0 = lam_Nt)
vr_Nt <- calibrate_params(inits = inits, f = f, s = s, lam0 = lam_Nt)


s = s_calibrated
f = f_calibrated

diff_rel_lam = 0.04


##

survivals = s_calibrated
fecundities = f_calibrated

##

fatalities = M; onset_time = onset_time; intial_pop_vector = N0; s = s; f = f;
model_demo = model_demo; time_horzion = time_horzion;
coeff_var_environ = coeff_var_environ; fatal_constant = fatal_constant

##

N1 = N[,t-1,j]
s = ss
f = ff
h = h[j]

##











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
