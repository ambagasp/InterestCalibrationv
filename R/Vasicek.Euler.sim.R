#' Vasicek Euler-Maruyama discretization approach.
#'
#' This function simulate M  interest rate paths from the Vasicek model using Euler-Maruyama discretization approach
#'
#' @param t0 starting point of interest rate path.
#'
#' @param T ending time point.
#'
#'both t0 and T are given in years.
#'
#'@param Delta is the time step; default value being 1 day (1/252 in years).
#'
#'@param r0 is the starting value of the path.
#'
#'@param gamma is speed of mean reversion
#'
#'@param rbar is the long term mean.
#'
#'@param sigma is the instantaneous volatility.
#'
#'@param M is the required number of interest rate paths

#'Default values of r0, gamma, rbar and alpha are the ones given in Veronesi(2010) Table 15.3 real world parameters.
#'@export
Vasicek.Euler.sim= function(t0=0,T=10,Delta=1/252,r0=0.03,
                            gamma=0.3262,rbar=0.0509,sigma=0.0221,M=10){
  N = ceiling((T-t0)/Delta)
  alpha = gamma*rbar*Delta
  sigmast = sigma*Delta^0.5
  beta = 1- gamma*Delta
  Z = matrix(rnorm(N*M,mean=0,sd=sigmast),nrow=N,ncol=M)
  X = matrix(0,nrow=N+1,ncol=M)
  X[1,]= r0
  for (i in 2:(N+1))
    X[i,] = alpha + beta*X[i-1,]+ Z[i-1,]
  X = ts(X,start=t0,deltat=Delta)
  return(invisible(X))
}
