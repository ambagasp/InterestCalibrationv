#' Function to calculate DELTA of a call option
#'
#' @param r0 short  rate at time 0
#' @param TO option maturity time
#' @param TB zero coupon bond maturity time
#' @param K option strike price as a percentage of face value
#'@param gamma is speed of mean reversion
#'@param rbar is the long term mean.
#'@param sigma is the instantaneous volatility.
#'
#'
#'@export
Vasicek.call.delta = function(r0=0.03,TO=0.5,TB=1,K=0.98,gamma=0.4653,rbar=0.0634,sigma=0.0221){
  SZ = 1/gamma*(1-exp(-gamma*(TB-TO)))*(sigma^2*(1-exp(-2*gamma*TO))/(2*gamma))^0.5
  ZCBTB= Vasicek.zcbp1(r0,t=0,T=TB,gamma,rbar,sigma)
  ZCBTO= Vasicek.zcbp1(r0,t=0,T=TO,gamma,rbar,sigma)
  d1 = 1/SZ*log(ZCBTB[1,4]/(K*ZCBTO[1,4]))+SZ/2
  d2 =d1 -SZ
  Delta = -ZCBTB[1,3]*ZCBTB[1,4]*pnorm(d1)+K*ZCBTO[1,3]*ZCBTO[1,4]*pnorm(d2)-
    ZCBTB[1,4]*dnorm(d1)*(ZCBTB[1,3]-ZCBTO[1,3])/SZ+
    K*ZCBTO[1,4]*dnorm(d2)*(ZCBTB[1,3]-ZCBTO[1,3])/SZ
  return(Delta)
}
