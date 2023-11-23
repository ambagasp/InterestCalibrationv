#' Call option price based on brute force simulation with the Vasicek model
#'
#' It uses transition density method of simulating interest rate paths.

#' @param r0 short  rate at time 0
#' @param TO option maturity time
#' @param TB zero coupon bond maturity time
#' @param K option strike price as a percentage of face value
#' @param Delta time step for simulation
#' @param M is the number of paths to be simulated
#'@param gamma is speed of mean reversion
#'@param rbar is the long term mean.
#'@param sigma is the instantaneous volatility.
#'
#' default \eqn{\gamma,\bar{r}} and \eqn{\sigma} are the values in example 15.1
#' default Delta is one day in years
#' @export

Vasicek.sim.call.zcb = function(r0=0.03,TO=0.5,TB=1,K=0.98,gamma=0.4653,rbar=0.0634,sigma=0.0221,Delta=1/252,M=30){
  X=Vasicek.Trans.sim(t0=0,T=TO,Delta,r0,gamma,rbar,sigma,M)
  N = ceiling(TO/Delta) # Number of points in time to option expiry
  ZTO = exp(-colSums(X[1:N,])*Delta)  # prices of zero coupon bonds maturing at time T0
  ZTOTB = rep(0,M)  # place holder for forward bond prices at time TO for a bond maturing at time TB
  N = ceiling((TB-TO)/Delta) # revised delta values
  for (j in 1:M){
    Y=Vasicek.Trans.sim(t0=TO,T=TB,Delta,r0=X[N+1,j],gamma,rbar,sigma,M)
    ZTOTB[j]= mean(exp(-colSums(Y[1:N,])*Delta))
  }
  return(mean(ZTO*pmax(ZTOTB-K,0)))
}
