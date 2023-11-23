#' CIR path simulation using transition density approach.
#'
#' This function simulate M  interest rate paths from the CIRmodel using transition density approach
#'
#' @param t0 starting point of interest rate path.
#'
#' @param T ending time point.
#'
#'both t0 and T are given in years.
#'
#'@param Delta is the time step; default value being 1 day (1/252 in years).
#'
#'
#'@param r0 is the starting value of the path
#'
#'@param gamma is the mean reversion speed
#'
#'@param rbar is the long term mean
#'
#'@param alpha rbar*gamma> alpha/2 not to have negative rates
#'
#'@param M is the number of paths to be simulated
#'
#'Default values of r0, gamma, rbar and alpha are the ones given in Veronesi(2010) page 552 footnote 10.
#'@export
CIR.Trans.sim= function(t0=0,T=1,Delta=1/252,r0=0.02,
                        gamma=0.3807,rbar=0.072,alpha=0.0548,M=10){
  N= ceiling((T-t0)/Delta)
  X = matrix(0,nrow=N+1,ncol=M)
  X[1,]= r0
  cs = 4*gamma/(alpha*(1-exp(-gamma*Delta)))
  nu = 4*gamma*rbar/alpha
  lambda = cs*exp(-gamma*Delta)
  for (i in 2:(N+1))
    X[i,]=rchisq(M,df=nu,ncp=lambda*X[i-1,])/cs
  X = ts(X,start=t0,deltat=Delta)
  return(invisible(X))
}
