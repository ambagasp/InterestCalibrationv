#' Vasicek negative probability
#'
#'This function calculates the probability of next value become negative in the Vasicek path
#'It calculates conditional probability \eqn{P[r_{t+s}<0|r_t]}
#' @param rt is the current value
#'
#' @param s is the time step
#'
#' @param gamma is speed of mean reversion
#'
#'@param rbar is the long term mean.
#'
#'@param sigma is the instantaneous volatility.
#'@export
Vasicek.neg.prob =function(s=1/252,rt=0.03,
                           gamma=0.3262,rbar=0.05,sigma=0.0221)
{
  m1 = rbar + (rt-rbar)*exp(-gamma*s)
  var1 = sigma^2*(1-exp(-2*gamma*s))/(2*gamma)
  z1 = -m1/var1^0.5
  return(pnorm(z1))
}

