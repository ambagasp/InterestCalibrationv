#' Function to calculate log of probability density function.
#'
#' It calculates the log of the following density function.
#' \deqn{f(r_{t+s}|r_t) = c_s \chi^2(c_sr_{t+s},\nu,\lambda_{t+s})}
#' where \eqn{\chi^2(.,\nu,\lambda_{t+s})} is a noncentral \eqn{\chi^2} density function  with \eqn{\nu} degree of freedom,
#' and non centrality parameter \eqn{\lambda_{t+s}}, with
#' \deqn{c_s = \frac{4\gamma}{\alpha(1-\exp(-\gamma s))}}
#' \deqn{\nu = \frac{4\gamma}{\alpha} \bar{r}}
#' \deqn{\lambda_{t+s} = c_sr_t \exp(-\gamma s)}
#'
#'
#'@param x0 current observation
#'
#'@param x next observation
#'
#' @param Delta is the time step; default value being 1 day (1/252 in years).
#'
#'
#'@param gamma is the mean reversion speed
#'
#'@param rbar is the long term mean
#'
#'@param alpha rbar*gamma> alpha/2 not to have negative rates
#'
#'@export

CIR.log.density = function(x,Delta,x0,gamma,rbar,alpha){

  cDelta = 4* gamma/(alpha*(1-exp(-gamma*Delta)))
  nu = 4*gamma/alpha*rbar
  lambda = cDelta*x0*exp(-gamma*Delta)
  log.lik = log(cDelta)+ dchisq(cDelta*x,df=nu,ncp=lambda,log=TRUE)
  return(log.lik)
}
