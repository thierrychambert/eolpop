
S = 0.05
rbinom(3, size = 1, prob = S-trunc(S))

which(is.na(N), arr.ind = TRUE)

N[,,2,498]




s = c(0.5, 0.75, 0.9)
f = c(0, 0.2, 0.4)
lambda(build_Leslie(s,f))

nac = 3
N1 = c(0,0,0)
h = 0.03
s_corr_factor = f_corr_factor = 1.2

# Survivors using the "s_realized" from reference scenarios
S <- N1*(1-h)*s*s_corr_factor

# Active rounding
S <- round(trunc(S) + rbinom(nac, size = 1, prob = S-trunc(S)))

N2 <- c(rep(0, nac-1), tail(S,1)) + c(0, head(S,-1))

# Births
B <- sum(f*f_corr_factor*N2)

# Active rounding
B <- round(trunc(B) + rbinom(1, size = 1, prob = B-trunc(B)))

N2[1] <- B
N2
