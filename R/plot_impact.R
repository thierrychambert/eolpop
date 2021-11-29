##==============================================================================
##                        Plot of the relative impact                         ==
##==============================================================================
#' Plot the relative impact for each scenario
#'
#' @param N a 4-D array containing demographic projection outputs
#' @param onset_year a vector containing the years of each wind farm start being active
#' (thus, the year at whihc each fatality value starts kicking in)
#' @param sel_sc scenario to display on the plot. Either "all" or the ID number of a given scenario.
#' @param percent a logical value indicating whether the impact should be displayed in % (y axis).
#' If FALSE, the impact value displayed is between 0 and -1 (negative impact).
#' @param show_CI value between 0 and 1. The limits of C.I. to calculate
#' @param xlab a character string. Label for the x axis.
#' @param ylab a character string. Label for the y axis.
#' @param Legend a vector of character strings. The legend to show on the side of the plot.
#' @param legend_position Position of the legend : "bottom", "right", etc. See option in ggplot2.
#' @param text_size Size of labels and legend text. Either "large" or "small".
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
plot_impact <- function(N, onset_year = NULL, sel_sc = "all", percent = TRUE, show_CI = 0.95, xlab = "Year", ylab = "Relative impact (%)",
                        Legend = NULL, legend_position = "right", text_size = "large", ...){

  # select subset of legends, if needed
  if(sel_sc != "all") sel_sc = as.numeric(sel_sc)
  if(sel_sc != "all") Legend = Legend[c(1, sel_sc+1)]

  # Get metrics and dimensions
  out <- get_metrics(N)$scenario$DR_N
  out[which(is.nan(out))] <- -1

  if(percent) out <- out*100

  TH <- dim(N)[2]
  nsc <- dim(N)[3]
  if(is.null(onset_year)) onset_year <- 1
  years <- min(onset_year) + (1:TH) - 1

  CI <- apply(out[,,], c(1,3), quantile, probs = c(0.5, 1-(1-show_CI)/2, (1-show_CI)/2))
  rownames(CI) <- c("avg", "lci", "uci")


  # Build dataframe
  if(sel_sc == "all"){
    df <- as.data.frame(cbind(year = years, t(CI[,,1]), scenario = 1))
    for(j in 2:nsc) df <- rbind(df, cbind(year = years, t(CI[,,j]), scenario = j))
  }else{
    df <- as.data.frame(cbind(year = years, t(CI[,,1]), scenario = 1))
    df <- rbind(df, cbind(year = years, t(CI[,,sel_sc+1]), scenario = sel_sc+1))
  }

  ## Define Graphic Parameters
  size = 1.5


  # Plot lines
  p <-
    ggplot(data = df, aes(x = .data$year, y = .data$avg)) +
    geom_line(size = size, aes(colour = factor(.data$scenario))) +
    geom_ribbon(
      aes(ymin = .data$uci, ymax = .data$lci, fill = factor(.data$scenario)), linetype = 0, alpha = 0.100)

  # change color palette (we want sc0 in black)
  if(sel_sc == "all") ColoR <- custom_palette_c25()[1:nsc] else ColoR <- custom_palette_c25()[c(1, sel_sc + 1)]

  p <- p +
    scale_color_manual(values = ColoR,
                       labels = Legend, aesthetics = c("colour", "fill"))


  # Add x/y labels and legend
  if(text_size == "large"){
    ts1 = 20
    ts2 = 18
    ts3 = 14
    u1 = 2
    u2 = 3
  }else{
    ts1 = 12
    ts2 = 12
    ts3 = 10
    u1 = 1
    u2 = 1.5
  }

  p <- p +
    labs(x = xlab, y = ylab,
         col = "Scenario", fill = "Scenario") +
    theme(
      axis.title=element_text(size = ts1, face = "bold"),
      axis.text=element_text(size = ts3)
    ) +

    theme(legend.position = legend_position,
          legend.key.height = unit(u1, 'line'),
          legend.key.width = unit(u2, 'line'),
          legend.title = element_text(size = ts2, face = "bold"),
          legend.text = element_text(size = ts3))

  # Add y-axis on right side, and make pretty x/y axis and limits
  p <- p +
    scale_y_continuous(limits = c(-100,0), expand = expansion(mult = c(0.015, 0.005)),
                       breaks = scales::pretty_breaks(n = 10),
                       sec.axis = sec_axis(trans = ~.*1, name = "",
                                           breaks = scales::pretty_breaks(n = 10))) +
    scale_x_continuous(expand = expansion(mult = c(0.015, 0)),
                       breaks = scales::pretty_breaks(n = 10))

  # Add horizontal dashed lines (for better viz)
  p <- p + geom_hline(yintercept = seq(0 , -100, by = -10), size = 0.5, linetype = 3, colour = grey(0.15))



  return(p)

} # End function
################################################################################



