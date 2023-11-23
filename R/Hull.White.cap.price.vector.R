#' Calculate cap price vector for Hull-White model.
#' 
#' This function uses the function Hull.White.cap.
#' 
#' Parameters are identical to that of cap price in the same model.
#' 
#' @param cap_rates cap rates starting at 0.25 going up to the largest cap
#' @param Maturity vector containing 0.25,0.5,0.75 etc
#' @param Delta reciprocal of number of payments per year; it is assumed 4 times payments, Delta =0.25
#' @param discount_factors for times 0.25, 0.5,0.75...
#' @param gamma speed of mean reversion 
#' @param sigma Hull-White instantaneous volatility which is constant
#'@export
Hull.White.cap.price.vector = function(cap_rates,Maturity,Delta=0.25,discount_factors,gamma,sigma){
  n = length(Maturity)-1
  caps =rep(0,n+1)
  for (i in 2:(n+1)){
    caps[i] = Hull.White.cap(rK=cap_rates[i],TK=Maturity[i],Delta=Delta,discount_factors =discount_factors,gamma=gamma,sigma=sigma)
  }
  return(caps)
}
