% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_simul.R
\name{run_simul}
\alias{run_simul}
\title{Run multiple population projections (simulations)}
\usage{
run_simul(
  nsim,
  cumulated_impacts,
  fatalities_mean,
  fatalities_se,
  onset_time,
  pop_size_mean,
  pop_size_se,
  pop_size_type,
  pop_growth_mean,
  pop_growth_se,
  survivals,
  fecundities,
  carrying_capacity_mean,
  carrying_capacity_se,
  theta = 1,
  rMAX_species,
  model_demo = NULL,
  time_horizon,
  coeff_var_environ,
  fatal_constant
)
}
\arguments{
\item{nsim}{number of simulation}

\item{cumulated_impacts}{Logical. If TRUE, we used the projection model for cumulated impacts.}

\item{fatalities_mean}{a vector (numeric). Average number of fatalities, for each scenario.}

\item{fatalities_se}{a vector (numeric). Standard Error for the number of fatalities, for each scenario (= uncertainties around the values provided).}

\item{onset_time}{a vector (numeric). The times at which each wind farm fatality starts applying.}

\item{pop_size_mean}{a single number. Average population size (either total population or number of pairs - see Ntype below).}

\item{pop_size_se}{Standard Error for population size (= uncertainty around the value provided).}

\item{pop_size_type}{character value indicating if the provided value pop_size correpsonds to Total Population Size ("Ntotal")
or the Number of Pairs ("Npair"). A stable age distribution is used to infer the size of each age class.}

\item{pop_growth_mean}{a number. Average population growth rate (lambda).}

\item{pop_growth_se}{Standard Error for population growth rate (= uncertainty around the value provided).}

\item{survivals}{a vector. Average survival probabilities for each age class.}

\item{fecundities}{a vector of fecundity values for each age class.}

\item{carrying_capacity_mean}{a strictly positive number.
Average carrying capacity (= maximum size that the population can reach). Here, the unit is the same as pop_size_type.
It can thus be expressed as the total population or the number of pair.}

\item{carrying_capacity_se}{Standard Error for the carrying capacity.}

\item{theta}{a strictly positive number. Parameter defining the shape of the density-dependence relationship.
The relationship is defined as : r <- rMAX*(1-(N/K)^theta)
Note lambda = r + 1}

\item{rMAX_species}{the maximum value of rMAX for the species under consideration,
usually calculated using the Niel & Lebreton (2005) equation.
It can be calculated using the function rMAX_spp. See ?rMAX_spp for details.

References :
Niel, C., and J. Lebreton. 2005. Using demographic invariants to detect overharvested bird
populations from incomplete data. Conservation Biology 19:826-835.}

\item{model_demo}{is NULL, by default, because the model choice will be made inside each iteration (simulation),
base on the values of N0 and lam0 that are drawn.
But it can be forced by setting the value, which must then be an R object corresponding to the demographic model to be used.
The 4 possible models currently are: M1_noDD_noDemoStoch, M2_noDD_WithDemoStoch, M3_WithDD_noDemoStoch, M4_WithDD_WithDemoStoch,}

\item{time_horizon}{a number. The number of years (time horizon) over which to project the population dynamics.}

\item{coeff_var_environ}{a number. The coefficient of variation to model environment stochasticity.}

\item{fatal_constant}{text (character). Either "h" or "M". Using "h" sets the fatality RATE as the constant value across years.
Using "M" sets the NUMBER of fatalities as the constant value across years.}
}
\value{
a 4D array containing the size of each age class (dim 1), for each year (dim 2), each scenario (dim 3), and
each simulation iteration (dim 4)
}
\description{
Run multiple population projections (simulations)
}
\examples{
fatalities_mean = c(0, 5, 10, 15, 20)
fatalities_se = fatalities_mean*0.05

pop_size_mean = 200
pop_size_se = 30
pop_size_type = "Npair"

pop_growth_mean = 1
pop_growth_se = 0.03

survivals <- c(0.5, 0.7, 0.8, 0.95)

fecundities <- c(0, 0, 0.05, 0.55)


time_horizon = 30
coeff_var_environ = 0.10
fatal_constant = "h"

carrying_capacity_mean = 1200
carrying_capacity_se = 100
theta = 1
rMAX_species <- 0.15

run_simul(nsim = 10, cumulated_impacts = FALSE,
           fatalities_mean, fatalities_se, onset_time = NULL,
           pop_size_mean, pop_size_se, pop_size_type,
           pop_growth_mean, pop_growth_se,
           survivals, fecundities,
           carrying_capacity_mean, carrying_capacity_se,
           theta,
           rMAX_species,
           model_demo = NULL, time_horizon, coeff_var_environ, fatal_constant)


}
