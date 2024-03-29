% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_traj.R
\name{plot_traj}
\alias{plot_traj}
\title{Plot demographic trajectories}
\usage{
plot_traj(
  N,
  age_class_use = "NotJuv0",
  fecundities = NULL,
  onset_year = NULL,
  sel_sc = "all",
  xlab = "Year",
  ylab = "Population size",
  Legend = NULL,
  ylim = NULL,
  ...
)
}
\arguments{
\item{N}{a 4-D array containing demographic projection outputs}

\item{age_class_use}{Either : "NotJuv0", "all" or "pairs". Which age class should be included
in the population size count to be plotted. "NotJuv0"}

\item{fecundities}{values of fecundities for each age class. This information is only required
if age_class_use = "pairs", to determine which age classes are mature (thus contribute to the number of pairs).}

\item{onset_year}{a vector containing the years of each wind farm start being active
(thus, the year at whihc each fatality value starts kicking in)}

\item{sel_sc}{scenario to display on the plot. Either "all" or the ID number of a given scenario.}

\item{xlab}{a character string. Label for the x axis.}

\item{ylab}{a character string. Label for the y axis.}

\item{Legend}{a vector of character strings. The legend to show on the side of the plot.}

\item{ylim}{a vector of 2 numbers. Limits of  the y axis.}

\item{...}{any other graphical input similar to the R plot function}
}
\value{
a plot of the relative impact of each scenario.
}
\description{
Plot demographic trajectories
}
