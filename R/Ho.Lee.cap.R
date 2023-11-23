#' Price of a cap.
#' 
#' It is  calculated as a sum of individual caplet prices.
#'@param  TK is the last caplet payment.
#'@param rK cap rate
#' first caplet payment starts at time 2*Delta
#' discount_curve must have discount factors for time points: Delta, 2*Delta,3*Delta,...T2
#' calculate values of all the caplet that makes payment from 2*Delta to T2 and add them up
#'@param Delta reciprocal of number of payments pere year; it is assumed 4 times payments, Delta =0.25
#'@param discount_factors for times 0.25, 0.5,0.75...
#'@param sigma Ho-lee  instantaneous volatility which is a constant
#'@export
Ho.Lee.cap = function(rK=0.05,TK=5,Delta=0.25,discount_factors,sigma){
  no.caplets = TK/Delta -1
  sum(Ho.Lee.caplet(rK=rK,T=seq(2*Delta,TK,Delta),Delta=Delta,
                    ZT1=discount_factors[1:no.caplets],
                    ZT2=discount_factors[2:(no.caplets+1)],
                    sigma=sigma))
}
