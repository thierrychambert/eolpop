##==============================================================================
##                           Plot trajectories                                ==
##==============================================================================
#' Plot demographic trajectories
#'
#' @param N a 4-D array containing demographic projection outputs
#' @param onset_year a vector containing the years of each wind farm start being active
#' (thus, the year at whihc each fatality value starts kicking in)
#' @param percent a logical value indicating whether the impact should be displayed in % (y axis).
#' If FALSE, the impact value displayed is between 0 and -1 (negative impact).
#' @param xlab a character string. Label for the x axis.
#' @param ylab a character string. Label for the y axis.
#' @param ... any other graphical input similar to the R plot function
#'
#' @return a plot of the relative impact of each scenario.
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom scales pretty_breaks
#' @import ggplot2
#'
#' @examples
#' # plot_impact(demo_proj, xlab = "year", ylab = "pop size")
#'
plot_impact <- function(N, onset_year = NULL, percent = TRUE, xlab = "Year", ylab = "Relative impact (%)", ...){

  # Get metrics and dimensions
  if(percent) out <- get_metrics(N)$scenario$impact*100 else out <- get_metrics(N)$scenario$impact
  TH <- dim(N)[2]
  nsc <- dim(N)[3]
  if(is.null(onset_year)) onset_year <- 1
  years <- min(onset_year) + (1:TH) - 1

  # Build dataframe
  df <- as.data.frame(cbind(year = years, out[,,1], scenario = 1))
  for(j in 2:nsc) df <- rbind(df, cbind(year = years, out[,,j], scenario = j))

  ## Define Graphic Parameters
  size = 1.5


  # Plot lines
  p <-
    ggplot(data = df, aes(x = .data$year, y = .data$avg)) +
    geom_line(data = dplyr::filter(df, .data$scenario > 1), size = size, aes(colour = factor(.data$scenario))) +
    geom_line(data = dplyr::filter(df, .data$scenario == 1), size = size, colour = "black")

  # Plot CIs
  p <- p +
    geom_ribbon(data = filter(df, .data$scenario > 1),
                aes(ymin = .data$uci, ymax = .data$lci, fill = factor(.data$scenario)), linetype = 0, alpha = 0.100)

  # Add x/y labels and legend
  Legend <- "... + Parc"
  nsc <- max(df$scenario) - 1

  p <- p +
    labs(x = xlab, y = ylab,
         col = "Scenario", fill = "Scenario") +
    theme(
      axis.title=element_text(size = 20, face = "bold"),
      axis.text=element_text(size = 14)
    ) +

    scale_color_hue(labels = c("Parc 1",paste(Legend, 2:nsc)), aesthetics = c("colour", "fill")) +
    theme(legend.key.height = unit(2, 'line'),
          legend.key.width = unit(3, 'line'),
          legend.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 14))

  # Add y-axis on right side, and make pretty x/y axis and limits
  p <- p +
    scale_y_continuous(limits = c(-100,0), expand = expansion(mult = c(0.015, 0.005)),
                       breaks = scales::pretty_breaks(n = 10),
                       sec.axis = sec_axis(trans = ~.*1, name = "",
                                           breaks = scales::pretty_breaks(n = 10))) +
    scale_x_continuous(expand = c(0,0))

  # Add horizontal dashed lines (for better viz)
  p <- p + geom_hline(yintercept = seq(0 , -100, by = -10), size = 0.5, linetype = 3, colour = grey(0.15))



  return(p)

} # End function
################################################################################



