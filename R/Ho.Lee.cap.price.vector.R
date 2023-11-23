#' Calculate cap price vector.
#' 
#'This calculates a vector of cap prices and it uses Ho.Lee.cap function.
#' @param cap_rates cap rates starting at 0.25 going upto the largest cap
#' @param Maturity vector containing 0.25,0.5,0.75 etc
#' @param Delta reciprocal of number of payments per year; it is assumed 4 times payments, Delta =0.25
#' @param discount_factors for times 0.25, 0.5,0.75...
#' @param sigma Ho-lee  instantaneous volatility which is a constant
#'@export
Ho.Lee.cap.price.vector = function(cap_rates,Maturity,Delta=0.25,discount_factors,sigma){
  #browser()
  n = length(Maturity)-1
  caps =rep(0,n+1)
  for (i in 2:(n+1)){
    caps[i] = Ho.Lee.cap(rK=cap_rates[i],TK=Maturity[i],Delta=Delta,discount_factors,sigma)
  }
  return(caps)
}
