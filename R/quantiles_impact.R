#' Calculate quantile values of impact at time horizon
#'
#' @param dr_N a 3-D array (year, nsim, nsc) containing the output : relative difference of population size
#' @param show_quantile value between 0 and 1. The quantile to of the CDF to calculate
#' @param show_CI value between 0 and 1. The limits of C.I. to calculate
#' @param percent a logical value indicating whether the impact should be displayed in % (y axis).
#' If FALSE, the impact value displayed is between 0 and -1 (negative impact).
#'
#' @return the CI and quantiles values
#' @export
#'
#'
quantiles_impact <- function(dr_N, show_quantile = 0.975, show_CI = 0.95, percent = TRUE){

  dr_N[which(is.nan(dr_N))] <- -1
  if(percent) dr_N <- dr_N*100

  CI <- QT <- NA

  TH <- dim(dr_N)[1]
  nsc <- dim(dr_N)[3]

  if(!is.null(show_CI)) CI <- apply(-dr_N[TH,,], 2, quantile, probs = c(0.5, (1-show_CI)/2, 1-(1-show_CI)/2))
  if(!is.null(show_quantile)) QT <- apply(-dr_N[TH,,], 2, quantile, probs = show_quantile)

  CI[] <- sapply(CI, max, 0)
  QT[] <- sapply(QT, max, 0)

  return(list(CI = CI, QT = QT))
}

