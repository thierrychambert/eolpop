% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibrate_params.R
\name{calibrate_params}
\alias{calibrate_params}
\title{Calibration of vital rate values
Function to adjust the vital rates values in order to match the desired population growth rate}
\usage{
calibrate_params(inits = NULL, s, f, lam0)
}
\arguments{
\item{inits}{intial values of survival and fecundities for the optimization}

\item{s}{a vector of fecundity values for each age clas}

\item{f}{a vector of survival probabilities for each age class}

\item{lam0}{the desired population growth rate - the one to be matched}
}
\value{
a vector of adjusted values of survival and fecundities
}
\description{
Calibration of vital rate values
Function to adjust the vital rates values in order to match the desired population growth rate
}
\examples{
s <- c(0.5, 0.7, 0.8, 0.95)
f <- c(0, 0, 0.05, 0.55)
calibrate_params(inits = NULL, s = s, f = f, lam0 = 1.08)

}
