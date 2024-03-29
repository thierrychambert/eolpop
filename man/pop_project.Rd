% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pop_project.R
\name{pop_project}
\alias{pop_project}
\title{Population projection over time}
\usage{
pop_project(
  fatalities,
  intial_pop_vector,
  s,
  f,
  DD_params = NULL,
  model_demo,
  time_horizon,
  coeff_var_environ,
  fatal_constant = "h",
  onset_time = NULL
)
}
\arguments{
\item{fatalities}{a vector (numeric). Each value correspond to the number of fatalities for each scenario.
The number of scenario assesed corresponds to the size of that vector.}

\item{intial_pop_vector}{a vector (numeric). Initial size of each age class. Typically, the output of the
pop_vector function.}

\item{s}{a vector of survival probabilities for each age class}

\item{f}{a vector of fecundity values for each age class}

\item{DD_params}{NULL or a list. Density-dependence parameters (rMAX, K, theta). Only used in DD models M3 and M4.}

\item{model_demo}{an R object corresponding to the demographic model to be used. The 4 possible models currently are:
M1_noDD_noDemoStoch, M2_noDD_WithDemoStoch, M3_WithDD_noDemoStoch, M4_WithDD_WithDemoStoch,}

\item{time_horizon}{a number. The number of years (time horizon) over which to project the population dynamics.}

\item{coeff_var_environ}{a number. The coefficient of variation to model environment stochasticity.}

\item{fatal_constant}{text (character). Either "h" or "M". Using "h" sets the fatality RATE as the constant value across years.
Using "M" sets the NUMBER of fatalities as the constant value across years.}

\item{onset_time}{unused. Just here because it's required for cumulated impact and in higher level 'run_simul" function.}
}
\value{
a 3D array containing the size of each age class (dim 1), for each year (dim 2) and each scenario (dim 3).
}
\description{
Population projection over time
}
\examples{
s <- c(0.5, 0.7, 0.8, 0.95)
f <- c(0, 0, 0.05, 0.55)
N0 <- pop_vector(pop_size = 200, pop_size_type = "Npair", s, f)
pop_project(fatalities = c(0, 5, 10), intial_pop_vector = N0, s = s, f = f,
model_demo = M2_noDD_WithDemoStoch, time_horizon = 30,
coeff_var_environ = 0.1, fatal_constant = "h")

}
