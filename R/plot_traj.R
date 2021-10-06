##==============================================================================
##                           Plot trajectories                                ==
##==============================================================================
#' Plot demographic trajectories
#'
#' @param N a 4-D array containing demographic projection outputs
#' @param onset_year a vector containing the years of each wind farm start being active
#' (thus, the year at whihc each fatality value starts kicking in)
#' @param xlab a character string. Label for the x axis.
#' @param ylab a character string. Label for the y axis.
#' @param Legend a vector of character strings. The legend to show on the side of the plot.
#' @param ... any other graphical input similar to the R plot function
#'
#' @return a plot of the relative impact of each scenario.
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom scales pretty_breaks
#' @import ggplot2
#'
#'
#'
plot_traj <- function(N, onset_year = NULL, xlab = "Year", ylab = "Relative impact (%)",
                        Legend = NULL, ...){

  # Get metrics and dimensions
  TH <- dim(N)[2]
  nsim <- dim(N)[4]
  nsc <- dim(N)[3]
  if(is.null(onset_year)) onset_year <- 1
  years <- min(onset_year) + (1:TH) - 1

  # Average trajectory and CI limits (here we use CI = +/- 0.5*SE to avoid overloading the graph)
  out <- colSums(N[-1,,,])
  N_avg <- apply(out, c(1,2), mean)
  N_lci <- apply(out, c(1,2), quantile, prob = pnorm(0.5))
  N_uci <- apply(out, c(1,2), quantile, prob = pnorm(-0.5))

  # Build dataframe
  df <- as.data.frame(cbind(year = years, N_avg = N_avg[,1], N_lci = N_lci[,1], N_uci = N_uci[,1], scenario = 1))
  for(j in 2:nsc) df <- rbind(df, cbind(year = years, N_avg = N_avg[,j], N_lci = N_lci[,j], N_uci = N_uci[,j], scenario = j))

  ## Define Graphic Parameters
  size = 1.5

  # Plot lines and CIs
  p <-
    ggplot(data = df, aes(x = .data$year, y = .data$N_avg)) +
    geom_line(size = size, aes(colour = factor(.data$scenario))) +
    geom_ribbon(aes(ymin = .data$N_lci, ymax = .data$N_uci, fill = factor(.data$scenario)), linetype = 0, alpha = 0.100)

  ## If want to not show CI for sc0, use :
  #  geom_ribbon(data = dplyr::filter(df, .data$scenario > 1), aes(ymin = .data$N_lci, ymax = .data$N_uci, fill = factor(.data$scenario)), linetype = 0, alpha = 0.100)

  # change color palette (we want sc0 in black)
  p <- p +
    scale_color_manual(breaks = 1:nsc,
                       values = palette()[1:nsc],
                       labels = Legend, aesthetics = c("colour", "fill"))


  # Add x/y labels and legend
  p <- p +
    labs(x = xlab, y = ylab,
         col = "Scenario", fill = "Scenario") +
    theme(
      axis.title=element_text(size = 20, face = "bold"),
      axis.text=element_text(size = 14)
    )


  # Improve th eoverall look
  p <- p +
    theme(legend.key.height = unit(2, 'line'),
          legend.key.width = unit(3, 'line'),
          legend.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 14))


  # Add y-axis on right side, and make pretty x/y axis and limits
  p <- p +
    scale_y_continuous(expand = expansion(mult = c(0.025, 0.005)),
                       breaks = scales::pretty_breaks(n = 10),
                       sec.axis = sec_axis(trans = ~.*1, name = "",
                                           breaks = scales::pretty_breaks(n = 10))) +
    scale_x_continuous(expand = expansion(mult = c(0.015, 0)),
                       breaks = scales::pretty_breaks(n = 10))


  return(p)

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
