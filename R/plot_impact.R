##==============================================================================
##                           Plot trajectories                                ==
##==============================================================================
#' Plot demographic trajectories
#'
#' @param N a 4-D array containing demographic projection outputs
#' @param ... any other graphical input similar to the R plot function
#'
#' @return a plot of the relative impact of each scenario.
#' @export
#'
#' @examples
#' plot_impact(demo_proj, xlab = "year", ylab = "pop size")
#'
plot_impact <- function(N, ...){
  out <- get_metrics(N)$scenario$impact_sc
  TH <- dim(N)[2]
  nsc <- dim(N)[3]

  # Initiate plot
  x=1:nrow(out)
  plot(x = x, y = out[,"avg",1], type = 'n', ylim = c(min(out[,"uci",], na.rm = TRUE),0), ...)
  abline(h = -1, lwd = 3, lty = 2)

  for(j in 1:nsc){
    # Average Impact
    # points(x = x, y = out[,"avg",j], type = 'l', col=j, lwd=3)

    # Shaded area for CI
    polygon.x <- c(x, rev(x))
    polygon.y <- c(out[,"lci",j], rev(out[,"uci",j]))
    polygon(x=polygon.x, y=polygon.y, col=adjustcolor(j, alpha.f=0.1), border=NA)
    } # j

  # Redraw Average Impact on top
  for(j in 1:nsc){
    points(x = x, y = out[,"avg",j], type = 'l', col=j, lwd=3)
  } # j


} # End function
################################################################################



