##==============================================================================
##                        Get Output metrics                                  ==
##==============================================================================

#' Calculate Impact Metrics
#'
#' @description
#' get_metrics takes the output array of the simulations run and calculate two impact metrics :
#' (1) The relative difference (between scenarios) in population size at the time horizon, and
#' (2) The difference in terms of extinction probability.
#'
#' When used on a cumulated_impacts analysis, this function also calculates these two metrics
#'  for each individual wind farm.
#'
#'
#' @param N a 4-D array containing demographic projection outputs
#' @param cumulated_impacts Logical. Must be set to TRUE if the output array N corresponds to
#' a cumulated impacts demographic analysis (see ?run_simul).
#'
#' @return a list of metric outputs : mean, SD, 95% C.I. of the
#' @export
#'
#' @examples
#' ## Import Data
#' data("demo_proj")
#'
#' ## Calculate the metric
#' get_metrics(demo_proj, cumulated_impacts = FALSE)
#'
#'
get_metrics <- function(N, cumulated_impacts = FALSE){

  TH <- dim(N)[2]
  warning <- NULL

  ### Impact of each SCENARIO #####
  impact_sc <- array(NA, dim = c(dim(N)[2], 4, dim(N)[3]),
                     dimnames = list(paste0("year", 1:dim(N)[2]),
                                     c("avg", "se", "lci", "uci"),
                                     paste0("sc", (1:dim(N)[3])-1)
                     ))


  # Define reference population size (sc0)
  N_ref <- colSums(N[,,"sc0",])

  # Remove cases where pop size under sc0 is too small (N < 30)
  ## to get accurate proportion (avg) and uncertainty metrics
  N_ref[N_ref < 30] <- NaN

  ## Create a warning if the sample size (number of useable iterations for calculation) becomes too small
  spl_size <- apply(N_ref, 1,
        function(x) sum(!is.nan(x))
        )

  # Warning message, if required
  if (min(spl_size) < 200) warning <- paste0(
    "WARNING : small sample size to calculate metrics, starting on year ",
    min(which(spl_size < 200)),
    ", due to high extinction rate.
    Use more simulations to get accurate proportions and uncertainty metrics."
    )


  for(j in 1:dim(N)[3]){
    # Relative Difference of Population Size
    DR_N <- (colSums(N[,,j,]) - N_ref) / N_ref

    # Remove cases where impact > 0
    sel <- which(DR_N > 0, arr.ind = TRUE)
    sel2 <- unique(sel[sel[,"row"] > 5, "col"])
    if(length(sel2) > 0){
      DR_N <- DR_N[,-sel2]
    }

    # Impact metric : Average value
    impact_sc[,"avg",j] <- apply(DR_N, 1, mean, na.rm = TRUE)

    # Impact metric : SE
    impact_sc[,"se",j] <- apply(DR_N, 1, sd, na.rm = TRUE)

    # Impact metric : Upper and Lower Confidence Intervals for DR_N
    impact_sc[,"uci",j] <- apply(DR_N, 1, quantile, probs = 0.025, na.rm = TRUE)
    impact_sc[,"lci",j] <-
      apply(DR_N, 1, quantile, probs = 0.975, na.rm = TRUE) %>%
      sapply(min, 0)

  } # j


  ## Probability of extinction
  Pext_sc <- DR_Pext_sc <- NA

  Pext_ref <- mean(colSums(N[,TH,"sc0",]) == 0)

  for(j in 1:dim(N)[3]){

    Pext_sc[j] <- mean(colSums(N[,TH,j,]) == 0 | colSums(N[,TH,"sc0",]) == 0)
    DR_Pext_sc[j] <- (Pext_sc[j] - Pext_ref) / Pext_ref

    DR_Pext_sc[DR_Pext_sc == Inf] <- Pext_sc[DR_Pext_sc == Inf]
    DR_Pext_sc[is.nan(DR_Pext_sc)] <- 0

  } # j

  # Save scenario impacts into a list
  scenario_impacts <- list(
    impact = impact_sc,
    Pext = Pext_sc,
    DR_Pext = DR_Pext_sc)

  #####

  #===================================================================
  #               CASE: CUMULATED IMPACT RUN                        ==
  #===================================================================

  indiv_impacts <- list(impacts = NULL)

  ### Impact of each WIND FARM (only in case of a cumluted impact run)
  impact_indiv <- array(NA, dim = c(dim(N)[2], 4, dim(N)[3]),
                        dimnames = list(paste0("year", 1:dim(N)[2]),
                                        c("avg", "se", "lci", "uci"),
                                        paste0("sc", (1:dim(N)[3])-1)
                          ))

  Pext_indiv <- DR_Pext_indiv <- NA

  if(cumulated_impacts){

    ## Relative difference of population size
    impact_indiv[,,1] <- 0

    for(j in 2:dim(N)[3]){

      # Define reference population size (sc0)
      N_ref <- colSums(N[,,j-1,])

      # Remove cases where pop size under sc0 is too small (N < 30)
      ## to get accurate proportion (avg) and uncertainty metrics
      N_ref[N_ref < 30] <- NaN

      ## Create a warning if the sample size (number of useable iterations for calculation) becomes too small
      spl_size <- apply(N_ref, 1,
                        function(x) sum(!is.nan(x))
      )

      # Warning message, if required
      if (min(spl_size) < 200) warning <- paste0(
        "WARNING : small sample size to calculate metrics, starting on year ",
        min(which(spl_size < 200)),
        ", due to high extinction rate.
        Use more simulations to get accurate proportions and uncertainty metrics."
      )


      # Relative Difference of Population Size
      DR_N <- (colSums(N[,,j,]) - N_ref) / N_ref

      # Remove cases where impact > 0
      sel <- which(DR_N > 0, arr.ind = TRUE)
      sel2 <- unique(sel[sel[,"row"] > 5, "col"])
      if(length(sel2) > 0){
        DR_N <- DR_N[,-sel2]
      }

      # Remove rare cases where sc0 = 0 and sc1 > 0 (making DR = +Inf)
      impact_indiv[,"avg",j] <- apply(DR_N, 1, mean, na.rm = TRUE)
      impact_indiv[,"se",j] <- apply(DR_N, 1, sd, na.rm = TRUE)

      # Upper and Lower Confidence Intervals for DR_N
      impact_indiv[,"uci",j] <- apply(DR_N, 1, quantile, probs = 0.025, na.rm = TRUE)
      impact_indiv[,"lci",j] <-
        apply(DR_N, 1, quantile, probs = 0.975, na.rm = TRUE) %>%
        sapply(min, 0)

    } # j

    ## Probability of extinction
    Pext_indiv <- DR_Pext_indiv <- 0

    # for scenario 0
    Pext_indiv[1] <- mean(colSums(N[,TH,1,]) == 0)

    for(j in 2:dim(N)[3]){

      Pext_indiv[j] <- Pext_sc[j] - Pext_sc[j-1]

      Pext_ref <- Pext_sc[j-1]

      DR_Pext_indiv[j] <- Pext_indiv[j] / Pext_ref

    } # j

    Pext_indiv <- sapply(Pext_indiv, max, 0)
    DR_Pext_indiv <- sapply(DR_Pext_indiv, max, 0)

    # Save individual wind farm impacts into a list
    indiv_impacts <- list(
      impact = impact_indiv,
      Pext = Pext_indiv,
      DR_Pext = DR_Pext_indiv)


  } # end if "ci=umulated_impacts"



  return(
    list(
      scenario = scenario_impacts,
      indiv_farm = indiv_impacts,
      warning = warning
    )
  )

} # End function
################################################################################
