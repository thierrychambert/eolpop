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
#' @importFrom dplyr filter
#' @import ggplot2
#'
#' @examples
#' # plot_impact(demo_proj, xlab = "year", ylab = "pop size")
#'
plot_impact <- function(N, ...){

  # Get metrics and dimensions
  out <- get_metrics(N)$scenario$impact
  TH <- dim(N)[2]
  nsc <- dim(N)[3]


  # Build dataframe
  df <- as.data.frame(cbind(year = 1:nrow(out), out[,,1], scenario = 1))
  for(j in 2:nsc) df <- rbind(df, cbind(year = 1:TH, out[,,j], scenario = j))

  ## Define Graphic Parameters
  size = 1.5

  # Plot lines
  p <-
    ggplot(data = df, aes(x = .data$year, y = .data$avg)) +
    geom_line(data = dplyr::filter(df, .data$scenario > 1), size = size, aes(colour = factor(.data$scenario))) +
    geom_line(data = dplyr::filter(df, .data$scenario == 1), size = size, colour = "black")

  # Plot CIs
  p <- p + geom_ribbon(data = filter(df, .data$scenario > 1),
                       aes(ymin = .data$uci, ymax = .data$lci, fill = factor(.data$scenario)), linetype = 0, alpha = 0.100)

  # Add legend
  Legend <- "parc"
  nsc <- max(df$scenario)-1
  p <- p + labs(x = "Annee", y = "Impact relatif",
                col = "Scenario", fill = "Scenario") +
    scale_color_hue(labels = paste(Legend, 1:nsc), aesthetics = c("colour", "fill"))

  return(p)

} # End function
################################################################################



