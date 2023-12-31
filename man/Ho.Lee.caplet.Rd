% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ho.Lee.caplet.R
\name{Ho.Lee.caplet}
\alias{Ho.Lee.caplet}
\title{Price of a caplet.}
\usage{
Ho.Lee.caplet(
  rK = 0.03815,
  T = 5,
  Delta = 0.25,
  ZT1 = 0.835326,
  ZT2 = 0.8247441,
  sigma = 0.15
)
}
\arguments{
\item{rK}{cap rate.}

\item{T}{is the time of caplet payment.}

\item{Delta}{reciprocal of number of payments pere year; it is assumed 4 times payments, Delta =0.25.}

\item{ZT1}{is the discount factor for time T-Delta.}

\item{ZT2}{is the discount factor for time T.}

\item{sigma}{Ho-lee  instantaneous volatility which is a constant.}
}
\description{
This is done  by viewing caplet as a put option of on a zero-coupon bond.
}
\details{
#' first caplet payment starts at time 2*Delta
discount_curve must have discount factors for time points: Delta, 2*Delta,3*Delta,...T2
calculate values of all the caplet that makes payment from 2*Delta to T2 and add them up
}
