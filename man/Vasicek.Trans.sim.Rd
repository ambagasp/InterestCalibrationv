% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Vasicek.Trans.sim.R
\name{Vasicek.Trans.sim}
\alias{Vasicek.Trans.sim}
\title{Vasicek path simulation using Transition density  approach.}
\usage{
Vasicek.Trans.sim(
  t0 = 0,
  T = 10,
  Delta = 1/252,
  r0 = 0.03,
  gamma = 0.3262,
  rbar = 0.0509,
  sigma = 0.0221,
  M = 10
)
}
\arguments{
\item{t0}{starting point of interest rate path.}

\item{T}{ending time point.

both t0 and T are given in years.}

\item{Delta}{is the time step; default value being 1 day (1/252 in years).}

\item{r0}{is the starting value of the path.}

\item{gamma}{is speed of mean reversion}

\item{rbar}{is the long term mean.}

\item{sigma}{is the instantaneous volatility.}

\item{M}{is the required number of interest rate paths

Default values of r0, gamma, rbar and alpha are the ones given in Veronesi(2010) Table 15.3 real world parameters.}
}
\description{
This function simulate M  interest rate paths from the Vasicek model using transition desnity  approach.
}
