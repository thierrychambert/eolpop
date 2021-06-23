##==============================================================================
##                        Get Output metrics                                  ==
##==============================================================================

#' Calculate Impact Metric
#' Relative Difference of Population Size DR_N
#'
#' @param N a 4-D array containing demographic projection outputs
#'
#' @return a list of metric outputs : mean, SD, 95% C.I. of the
#' @export
#'
#' @examples
#' ## Import Data
#' data("demo_proj")
#'
#' ## Calculate the metric
#' get_metrics(demo_proj)
#'
get_metrics <- function(N){

  DR_N <- array(NA, dim = dim(N)[2:4],
                dimnames = list(paste0("year", 1:dim(N)[2]),
                                paste0("sc", (1:dim(N)[3])-1)
                ))

  impact <- array(NA, dim = c(dim(N)[2], 4, dim(N)[3]),
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
    #DR_N[,j,] <- (colSums(N[,,j,]) - colSums(N[,,"sc0",])) / colSums(N[,,"sc0",])

    # Remove rare cases where sc0 = 0 and sc1 > 0 (making DR = +Inf)
    #DR_N[,j,][DR_N[,j,] == Inf] <- NaN

    impact[,"avg",j] <- rowMeans(DR_N[,j,], na.rm = TRUE)
    impact[,"se",j] <- apply(DR_N[,j,], 1, sd, na.rm = TRUE)

    # Upper and Lower Confidence Intervals for DR_N
    impact[,"uci",j] <- apply(DR_N[,j,], 1, quantile, probs = 0.025, na.rm = TRUE)
    impact[,"lci",j] <-
      apply(DR_N[,j,], 1, quantile, probs = 0.975, na.rm = TRUE) %>%
      sapply(min, 0)

  } # j

  return(impact)

} # End function
################################################################################
