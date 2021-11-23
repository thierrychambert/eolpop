#' Calculate quantile values of impact at time horizon
#'
#' @param N a 4-D array containing demographic projection outputs
#' @param show_quantile value between 0 and 1. The quantile to of the CDF to calculate
#' @param show_CI value between 0 and 1. The limits of C.I. to calculate
#' @param percent a logical value indicating whether the impact should be displayed in % (y axis).
#' If FALSE, the impact value displayed is between 0 and -1 (negative impact).
#'
#' @return the CI and quantiles values
#' @export
#'
#'
quantiles_impact <- function(N, show_quantile = 0.975, show_CI = 0.95, percent = TRUE){

  if(percent) out <- get_metrics(N)$scenario$DR_N*100 else out <- get_metrics(N)$scenario$DR_N

  CI <- QT <- NA

  TH <- dim(N)[2]
  nsc <- dim(N)[3]

  if(!is.null(show_CI)) CI <- apply(-out[TH,,], 2, quantile, probs = c(0.5, (1-show_CI)/2, 1-(1-show_CI)/2))
  if(!is.null(show_quantile)) QT <- apply(-out[TH,,], 2, quantile, probs = show_quantile)

  return(list(CI = CI, QT = QT))
}

