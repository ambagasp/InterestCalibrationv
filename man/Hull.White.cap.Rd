% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hull.White.cap.R
\name{Hull.White.cap}
\alias{Hull.White.cap}
\title{Price of the cap is calculated as a sum of caplet prices.}
\usage{
Hull.White.cap(rK = 0.05, TK = 5, Delta = 0.25, discount_factors, gamma, sigma)
}
\arguments{
\item{rK}{cap rate}

\item{TK}{is the time of last payment from the cap}

\item{Delta}{reciprocal of number of payments pere year; it is assumed 4 times payments, Delta =0.25}

\item{discount_factors}{for times 0.25, 0.5,0.75...TK}

\item{gamma}{Hull-White speed of mean reversion}

\item{sigma}{Hull-White  instantaneous volatility which is a constant}
}
\description{
TK is the last caplet payment.
}
\details{
first caplet payment starts at time 2*Delta.

discount_curve must have discount factors for time points: Delta, 2*Delta,3*Delta,...T2.

calculate values of all the caplet that makes payment from 2*Delta to T2 and add them up.
}
