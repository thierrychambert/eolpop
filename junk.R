##
survivals <- c(0.5, 0.7, 0)
fecundities <- c(0, 0.2, 0.55)
lam0 = 1.08
inits <- init_calib(s = survivals, f = fecundities, lam0 = lam0)
inits


s = survivals
f = fecundities
##




A00 <- build_Leslie(s=s, f=f)

# Difference to apply on lambda
diff_rel_lam <- (lam0 - lambda(A00))/lambda(A00)
d <- match_lam_delta(diff_rel_lam = diff_rel_lam, s=s, f=f)

el <- elements_Leslie(s=s, f=f)
vr0 = el$vital_rates
vr1 = vr0*(1+d)

nac = length(s)

# Define s_init and f_init
s_init <- head(vr1, nac)
f_init <- tail(vr1, nac)

# Apply correction to respect relative order of survivals and fecundities
s_init <- apply(cbind(s_init, s_init * (s/s[1]) / (s_init/s_init[1])), 1 , max, na.rm = TRUE)
f_init <- f_init * (f/max(f[f!=0])) / (f_init/max(f_init[f_init!=0]))
f_init[is.nan(f_init)] <- 0

# Calibrate survivals
#s_init <- (s_init %>% sapply(., max, 0.05)) %>% sapply(., min, 0.97)
s_init <- cbind(
  (s_init %>% sapply(., max, 0.05)) %>% sapply(., min, 0.97),
  s*1.1) %>%
  apply(., 1, min)


# Calibrate fecundities
f_init <- f_init %>% sapply(., max, 0.001)
f_init[f == 0] <- 0

# Combine vital rates
inits_vr <- c(s_init,f_init)
inits_vr <- c(tail(inits_vr, nac), head(inits_vr, nac))

# remove fecundities = 0 (but not survivals = 0, in case there is any)
inits <- inits_vr[-which(inits_vr == 0 & (substr(names(inits_vr),1,1) == "f"))]
inits

## old one
inits_vr[inits_vr != 0]
