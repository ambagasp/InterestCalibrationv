#' Price of the cap is calculated as a sum of caplet prices.
#' 
#' TK is the last caplet payment.
#' 
#' first caplet payment starts at time 2*Delta.
#' 
#' discount_curve must have discount factors for time points: Delta, 2*Delta,3*Delta,...T2.
#' 
#' calculate values of all the caplet that makes payment from 2*Delta to T2 and add them up.
#' 
#'@param rK cap rate
#'@param Delta reciprocal of number of payments pere year; it is assumed 4 times payments, Delta =0.25
#'@param discount_factors for times 0.25, 0.5,0.75...TK
#'@param sigma Hull-White  instantaneous volatility which is a constant
#'@param gamma Hull-White speed of mean reversion
#'@param TK is the time of last payment from the cap
#'@export
Hull.White.cap = function(rK=0.05,TK=5,Delta=0.25,discount_factors,gamma,sigma){

  no.caplets = TK/Delta -1

  sum(Hull.White.caplet(rK=rK,T=seq(2*Delta,TK,Delta),Delta=Delta,
                        ZT1=discount_factors[1:no.caplets],
                        ZT2=discount_factors[2:(no.caplets+1)],gamma,
                        sigma))
}
