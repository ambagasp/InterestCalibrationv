#'Function to calculate CIR negative probabilities
#'
#' This function calculates the probability of next value become negative in the 
#' CIR path under Euler discretization.
#' It calculates conditional probability \eqn{P[r_{t+s}<0|r_t]}
#'
#'@param s is the time step; default value being 1 day (1/252 in years).
#'
#'
#'@param rt is the current value
#'
#'@param gamma is the mean reversion speed
#'
#'@param rbar is the long term mean
#'
#'@param alpha rbar*gamma> alpha/2 not to have negative rates
#'
#'
#'@export
CIR.neg.prob =function(s=1/252,rt=0.2/100,
                       gamma=0.3262,rbar=0.07,alpha=0.0584){
  alpha1 = gamma*rbar*s
  sigma = (alpha*s)^0.5
  beta1 = 1- gamma*s
  return(pnorm(-(alpha1+beta1*rt)/(rt^0.5*sigma)))
}
