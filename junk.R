


### Impact of each SCENARIO
## Relative difference of population size
DR_N <- array(NA, dim = dim(N)[2:4],
              dimnames = list(paste0("year", 1:dim(N)[2]),
                              paste0("sc", (1:dim(N)[3])-1)
              ))

impact_sc <- array(NA, dim = c(dim(N)[2], 4, dim(N)[3]),
                dimnames = list(paste0("year", 1:dim(N)[2]),
                                c("avg", "se", "lci", "uci"),
                                paste0("sc", (1:dim(N)[3])-1)
                ))


# Define reference population size (sc0)
N_ref <- colSums(N[,,"sc0",])

# Remove cases where sc0 = 0
N_ref[N_ref == 0] <- NaN

for(j in 1:dim(N)[3]){
  # Relative Difference of Population Size
  DR_N[,j,] <- (colSums(N[,,j,]) - N_ref) / N_ref

  # Remove rare cases where sc0 = 0 and sc1 > 0 (making DR = +Inf)
  impact_sc[,"avg",j] <- apply(DR_N[,j,], 1, mean, na.rm = TRUE)
  impact_sc[,"se",j] <- apply(DR_N[,j,], 1, sd, na.rm = TRUE)

  # Upper and Lower Confidence Intervals for DR_N
  impact_sc[,"uci",j] <- apply(DR_N[,j,], 1, quantile, probs = 0.025, na.rm = TRUE)
  impact_sc[,"lci",j] <-
    apply(DR_N[,j,], 1, quantile, probs = 0.975, na.rm = TRUE) %>%
    sapply(min, 0)

} # j


## Probability of extinction
TH <- time_horzion

Pext_sc <- DR_Pext_sc <- NA

Pext_ref <- mean(colSums(N[,TH,"sc0",]) == 0)

for(j in 1:dim(N)[3]){

  Pext_sc[j] <- mean(colSums(N[,TH,j,]) == 0)
  DR_Pext_sc[j] <- (Pext_sc[j] - Pext_ref) / Pext_ref

} # j




if(cumuated_impacts){
  ### Impact of each WIND FARM
  ## Relative difference of population size
  DR_N <- array(NA, dim = dim(N)[2:4],
                dimnames = list(paste0("year", 1:dim(N)[2]),
                                paste0("sc", (1:dim(N)[3])-1)
                ))

  impact_indiv <- array(NA, dim = c(dim(N)[2], 4, dim(N)[3]),
                        dimnames = list(paste0("year", 1:dim(N)[2]),
                                        c("avg", "se", "lci", "uci"),
                                        paste0("sc", (1:dim(N)[3])-1)
                        ))

  impact_indiv[,,1] <- 0


  for(j in 2:dim(N)[3]){

    # Define reference population size (sc0)
    N_ref <- colSums(N[,,j-1,])

    # Remove cases where sc0 = 0
    N_ref[N_ref == 0] <- NaN

    # Relative Difference of Population Size
    DR_N[,j,] <- (colSums(N[,,j,]) - N_ref) / N_ref

    # Remove rare cases where sc0 = 0 and sc1 > 0 (making DR = +Inf)
    impact_indiv[,"avg",j] <- apply(DR_N[,j,], 1, mean, na.rm = TRUE)
    impact_indiv[,"se",j] <- apply(DR_N[,j,], 1, sd, na.rm = TRUE)

    # Upper and Lower Confidence Intervals for DR_N
    impact_indiv[,"uci",j] <- apply(DR_N[,j,], 1, quantile, probs = 0.025, na.rm = TRUE)
    impact_indiv[,"lci",j] <-
      apply(DR_N[,j,], 1, quantile, probs = 0.975, na.rm = TRUE) %>%
      sapply(min, 0)

  } # j

  ## Probability of extinction
  Pext_indiv <- DR_Pext_indiv <- 0

  Pext_indiv[1] <- mean(colSums(N[,TH,1,]) == 0)

  for(j in 2:dim(N)[3]){

    Pext_indiv[j] <- Pext_sc[j] - Pext_sc[j-1]

    Pext_ref <- Pext_sc[j-1]

    DR_Pext_indiv[j] <- Pext_indiv[j] / Pext_ref

  } # j

  Pext_indiv <- sapply(Pext_indiv, max, 0)
  DR_Pext_indiv <- sapply(DR_Pext_indiv, max, 0)


} # end if "ci=umulated_impacts"




impact_indiv[30,"avg",]
impact_sc[30,"avg",]

impact_indiv[30,"se",]
impact_sc[30,"se",]






















cumsum(impact_indiv[30,"avg",])

dim(N)
sc0 <- mean(colSums(N[,30,1,]))
sc1 <- mean(colSums(N[,30,2,]))
sc2 <- mean(colSums(N[,30,3,]))

(sc1-sc0)/sc0
(sc2-sc0)/sc0

(sc2-sc1)/sc1

(sc2-sc0)/sc0 + (sc2-sc1)/sc1

























# rm(list = ls(all.names = TRUE))
library(eolpop)
library(magrittr)
library(popbio)

s <- c(0.5, 0.7, 0.8, 0.95)
f <- c(0, 0, 0.05, 0.55)
N1 <- pop_vector(pop_size = 200, pop_size_type = "Npair", s, f)
h <- 0.05
DD_params <- list(rMAX = 0.15, K = 1200, theta = 1)





# Extract DD parameters from list
rMAX = DD_params$rMAX
K = DD_params$K
theta = DD_params$theta

rMAX = -10
# Apply density dependence effect
1 + rMAX*(1-(sum(N1)/K)^theta)

build_Leslie(s = s_calibrated, f = f_calibrated) %>% lambda



# Build Leslie matrix
A <- build_Leslie(s = s, f = f)

# Test if the overall lambda < 1 (lambda before applying density dependence)
if(lambda(A) < 1){

  # If lambda < 1, we cannot infer lambda[t]
  lam_t <- lambda(A)
}
