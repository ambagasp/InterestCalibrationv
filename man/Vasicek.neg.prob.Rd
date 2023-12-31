% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Vasicek.neg.prob.R
\name{Vasicek.neg.prob}
\alias{Vasicek.neg.prob}
\title{Vasicek negative probability}
\usage{
Vasicek.neg.prob(
  s = 1/252,
  rt = 0.03,
  gamma = 0.3262,
  rbar = 0.05,
  sigma = 0.0221
)
}
\arguments{
\item{s}{is the time step}

\item{rt}{is the current value}

\item{gamma}{is speed of mean reversion}

\item{rbar}{is the long term mean.}

\item{sigma}{is the instantaneous volatility.}
}
\description{
This function calculates the probability of next value become negative in the Vasicek path
It calculates conditional probability \eqn{P[r_{t+s}<0|r_t]}
}
