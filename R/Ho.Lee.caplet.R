#'Price of a caplet.
#'
#'This is done  by viewing caplet as a put option of on a zero-coupon bond.
#'
#'#' first caplet payment starts at time 2*Delta
#' discount_curve must have discount factors for time points: Delta, 2*Delta,3*Delta,...T2
#' calculate values of all the caplet that makes payment from 2*Delta to T2 and add them up
#' 
#'@param rK cap rate.
#'@param Delta reciprocal of number of payments pere year; it is assumed 4 times payments, Delta =0.25.
#'@param sigma Ho-lee  instantaneous volatility which is a constant.
#'@param T is the time of caplet payment.
#'@param  ZT1 is the discount factor for time T-Delta.
#'@param  ZT2 is the discount factor for time T.
#'@export
Ho.Lee.caplet = function(rK= 0.03815,T=5,Delta=0.25,ZT1=0.835326,ZT2=0.8247441,sigma=0.15){
  SZ = sigma*Delta*(T-Delta)^0.5
  K = 1/(1+rK*Delta)
  d1 = 1/SZ*log(ZT2/(K*ZT1))+ SZ/2
  d2 = d1-SZ
  return(ZT1*pnorm(-d2)-ZT2/K*pnorm(-d1))
}
