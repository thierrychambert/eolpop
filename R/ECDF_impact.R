#' Plot the estimated cumulative distribution function (ECDF) impact for each scenario
#'
#' @param N a 4-D array containing demographic projection outputs
#' @param show_quantile value between 0 and 1. The quantile to display on the plot
#' @param sel_sc scenario to display on the plot. Either "all" or the ID number of a given scenario.
#' @param xlims a vector of 2 values. x-axis limits (lower and upper).
#' @param percent a logical value indicating whether the impact should be displayed in % (y axis).
#' If FALSE, the impact value displayed is between 0 and -1 (negative impact).
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
ECDF_impact <- function(N, show_quantile = 0.95, sel_sc = "all", xlims = NULL,
                        percent = TRUE, xlab = "Relative impact (%)", ylab = "Cumulative density",
                        Legend = NULL, legend_position = "right", text_size = "large", ...){


  # select subset of legends, if needed
  if(sel_sc != "all") sel_sc = as.numeric(sel_sc)
  if(sel_sc != "all") Legend = Legend[sel_sc]

  # Get metrics and dimensions
  if(percent) out <- get_metrics(N)$scenario$DR_N*100 else out <- get_metrics(N)$scenario$DR_N
  TH <- dim(N)[2]
  nsc <- dim(N)[3]


  # Build dataframe
  if(sel_sc == "all"){
    df <- as.data.frame(cbind(impact = -out[TH,,2], scenario = 1))
    if(nsc > 2) for(j in 3:nsc) df <- rbind(df, cbind(impact = -out[TH,,j], scenario = j-1))
  }else{
    df <- as.data.frame(cbind(impact = -out[TH,,sel_sc+1], scenario = sel_sc))
  }


  ## Define Graphic Parameters
  size = 1.5


  # Plot lines
  p <- ggplot(df, aes(x = .data$impact)) +
    stat_ecdf(geom = "step", size = size, aes(colour = factor(.data$scenario)))

  # change color palette
  if(sel_sc == "all") ColoR <- custom_palette_c25()[2:nsc] else ColoR <- custom_palette_c25()[sel_sc + 1]

  p <- p +
    scale_color_manual(values = ColoR,
                       labels = Legend, aesthetics = c("colour"))


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
    scale_y_continuous(limits = c(0,1), expand = expansion(mult = c(0.015, 0.005)),
                       breaks = scales::pretty_breaks(n = 10),
                       sec.axis = sec_axis(trans = ~.*1, name = "",
                                           breaks = scales::pretty_breaks(n = 10))) +
    scale_x_continuous(limits = xlims, expand = expansion(mult = c(0.001, 0.001)),
                       breaks = scales::pretty_breaks(n = 10))


  # Add quantile vline
  QT <- apply(-out[TH,,], 2, quantile, probs = show_quantile, na.rm = TRUE)
  QT <- QT[-1]

  if(sel_sc != "all") QT <- QT[sel_sc]

  df_qt <- data.frame(x = QT, xend = QT, y = 0, yend = show_quantile)

  p <- p + geom_segment(data = df_qt,
                   mapping = aes(x = .data$x,
                                 xend = .data$xend,
                                 y = .data$y,
                                 yend = .data$yend),
                   color = ColoR, linetype="dashed", size=1)


  return(p)

} # End function
################################################################################



