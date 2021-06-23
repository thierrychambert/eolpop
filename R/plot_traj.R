##==============================================================================
##                           Plot trajectories                                ==
##==============================================================================
#' Plot demographic trajectories
#'
#' @param N a 4-D array containing demographic projection outputs
#' @param ... any other graphical input similar to the R plot function
#'
#' @return a plot of individual trajectories, from each simulation run,
#' and the average trajectory of each scenario.
#' Black = reference scenario (sc0). Red = fatality scenario (sc1)
#' @export
#'
#' @import dichromat
#'
#' @examples
#' plot_traj(demo_proj, xlab = "year", ylab = "pop size")
plot_traj <- function(N, ...){
  TH <- dim(N)[2]
  nsim <- dim(N)[4]

  # Average trend
  N_avg <- apply(N, c(1,2,3), mean)

  # Color palette
  col_sc0 <- make_transparent(colorRampPalette(colors = c("black", "grey"))(nsim), percent = 85)
  col_sc1 <- make_transparent(colorRampPalette(colors = c("red", "orange"))(nsim), percent = 85)

  # Initiate plot
  plot(x = 1:TH, y = colSums(N_avg[,,"sc0"]), type = 'n', col=1, lwd=3, ylim = c(0,max(colSums(N))), ...)

  ## Plot individual trajectories
  # Scenario 0
  for(sim in 1:nsim) points(x = 1:TH, y = colSums(N[,,"sc0",sim]), type = 'l', col=col_sc0[sim])

  # Scenario 1
  for(sim in 1:nsim) points(x = 1:TH, y = colSums(N[,,"sc1",sim]), type = 'l', col=col_sc1[sim])

  ## Plot average trend for each scenario
  # Scenario 0
  points(x = 1:TH, y = colSums(N_avg[,,"sc0"]), type = 'l', col=1, lwd=3)

  # Scenario 1
  points(x = 1:TH, y = colSums(N_avg[,,"sc1"]), type = 'l', col="red2", lwd=3)

} # End function
################################################################################




##==============================================================================
##                Function to control color transparency                      ==
##==============================================================================
#' Function to control color transparency on a plot
#'
#' @param someColor some color name, such as "green", "red", etc.
#' @param percent the desired % of transparency, between 0 (no transparency) and 100 (full transparency) .
#'
#' @return a Hex Code including the desired transparency level, such as #RRGGBB7F
#' @export
#'
#' @import RColorBrewer
#' @import grDevices
#'
#' @examples
#' make_transparent("green", percent=50)
make_transparent <- function(someColor, percent=100)
{
  newColor <- col2rgb(someColor)
  apply(newColor, 2,
        function(curcoldata){
          rgb(red = curcoldata[1], green = curcoldata[2], blue = curcoldata[3],
              alpha = (100 - percent) * 255 / 100, maxColorValue = 255)
        }
  )
} # End function
################################################################################
