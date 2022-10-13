##==============================================================================
##                           Plot trajectories                                ==
##==============================================================================
#' Plot demographic trajectories
#'
#' @param N a 4-D array containing demographic projection outputs
#' @param age_class_use Either : "NotJuv0", "all" or "pairs". Which age class should be included
#' in the population size count to be plotted. "NotJuv0"
#' @param fecundities values of fecundities for each age class. This information is only required
#' if age_class_use = "pairs", to determine which age classes are mature (thus contribute to the number of pairs).
#' @param onset_year a vector containing the years of each wind farm start being active
#' (thus, the year at whihc each fatality value starts kicking in)
#' @param sel_sc scenario to display on the plot. Either "all" or the ID number of a given scenario.
#' @param xlab a character string. Label for the x axis.
#' @param ylab a character string. Label for the y axis.
#' @param Legend a vector of character strings. The legend to show on the side of the plot.
#' @param ylim a vector of 2 numbers. Limits of  the y axis.
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
plot_traj <- function(N, age_class_use = "NotJuv0", fecundities = NULL, onset_year = NULL, sel_sc = "all",
                      xlab = "Year", ylab = "Population size", Legend = NULL, ylim = NULL, ...){

  # select subset of legends, if needed
  if(sel_sc != "all") sel_sc = as.numeric(sel_sc)
  if(sel_sc != "all") Legend = Legend[c(1, sel_sc+1)]

  # Get metrics and dimensions
  TH <- dim(N)[2]
  nsc <- dim(N)[3]
  nsim <- dim(N)[4]

  # Select which age classes to use for the plot
  # if(base::missing(age_class_use)) N <- N[-1,,,]
  if(match.arg(arg = age_class_use, choices = c("all", "NotJuv0", "pairs")) == "NotJuv0")  N <- N[-1,,,]
  if(match.arg(arg = age_class_use, choices = c("all", "NotJuv0", "pairs")) == "all")      N <- N
  if(match.arg(arg = age_class_use, choices = c("all", "NotJuv0", "pairs")) == "pairs"){
    if(is.null(fecundities)){
      stop("Argument 'fecundities' is required when age_class_use = 'pairs'")
    }else{
      mature <- which(fecundities != 0)
      N <- N[mature,,,]/2
    }
  }


  if(is.null(onset_year)) onset_year <- 1
  years <- min(onset_year) + (1:TH) - 1

  # Average trajectory and CI limits (here we use CI = +/- 0.5*SE to avoid overloading the graph)
  ## Here : it's total pop size WITHOUT Juv0
  if(length(dim(N)) == 3){
    out <- N
  }else{
    out <- colSums(N)
  }

  N_avg <- apply(out, c(1,2), median, na.rm = TRUE)
  N_lci <- apply(out, c(1,2), quantile, prob = pnorm(0.5), na.rm = TRUE)
  N_uci <- apply(out, c(1,2), quantile, prob = pnorm(-0.5), na.rm = TRUE)

  # Build dataframe
  if(sel_sc == "all"){
    df <- as.data.frame(cbind(year = years, N_avg = N_avg[,1], N_lci = N_lci[,1], N_uci = N_uci[,1], scenario = 1))
    for(j in 2:nsc) df <- rbind(df, cbind(year = years, N_avg = N_avg[,j], N_lci = N_lci[,j], N_uci = N_uci[,j], scenario = j))
  }else{
    df <- as.data.frame(cbind(year = years, N_avg = N_avg[,1], N_lci = N_lci[,1], N_uci = N_uci[,1], scenario = 1))
    df <- rbind(df, cbind(year = years, N_avg = N_avg[,sel_sc+1], N_lci = N_lci[,sel_sc+1], N_uci = N_uci[,sel_sc+1], scenario = sel_sc+1))
  }


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
  if(sel_sc == "all") ColoR <- custom_palette_c25()[1:nsc] else ColoR <- custom_palette_c25()[c(1, sel_sc + 1)]

  p <- p +
    scale_color_manual(values = ColoR,
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
                       limits = ylim,
                       sec.axis = sec_axis(trans = ~.*1, name = "",
                                           breaks = scales::pretty_breaks(n = 10))) +
    scale_x_continuous(expand = expansion(mult = c(0.015, 0)),
                       breaks = scales::pretty_breaks(n = 10))

  return(p)

} # End function
################################################################################

