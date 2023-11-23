#' Calculate cap price vector
#' 
#' The difference between this function and Hull.White.cap.price.vector is that this function uses something similar to 
#' forward volatilities.
#' 
#' @param cap_rates cap rates starting at 0.25 going up to the largest cap
#' @param Maturity vector containing 0.25,0.5,0.75 etc
#' @param Delta reciprocal of number of payments per year; it is assumed 4 times payments, Delta =0.25
#' @param discount_factors for times 0.25, 0.5,0.75...
#' @param sigma Hull-White forward volatility applicable to each point in Maturity
#'@export
Hull.White.cap.price.vectorv2 = function(cap_rates,Maturity,Delta=0.25,
                                         discount_factors,sigma){
  n = length(Maturity)-1
  caps =rep(0,n+1)
  for (i in 2:(n+1)){
    caps[i] = Hull.White.capv2(rK=cap_rates[i],TK=Maturity[i],
                               Delta=Delta,
                               discount_factors =discount_factors,
                               sigmaf=sigma[i])
  }
  return(caps)
}