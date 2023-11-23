#' Calculate cap price vector
#' 
#' This function uses the function Two.factor.Hull.White.cap.
#' 
#' Parameters are identical to that of cap price in the same model.
#' 
#' @param cap_rates cap rates starting at 0.25 going upto the largest cap
#' @param Maturity vector containing 0.25,0.5,0.75 etc
#' @param Delta reciprocal of number of payments per year; it is assumed 4 times payments, Delta =0.25
#' @param discount_factors for times 0.25, 0.5,0.75...
#' @param sigma1 first factor  instantaneous  volatility 
#' @param sigma2 second factor instantaneous  volatility 
#' @param gamma1 first factor speed of mean reversion
#' @param gamma2 second factor speed of mean reversion
#' @param rho instantaneous correlation coefficient between two factors
#'@export
Two.factor.Hull.White.cap.price.vector = function(cap_rates,Maturity,Delta=0.25,discount_factors,
                                                   gamma1,gamma2,sigma1,sigma2,rho){
  n = length(Maturity)-1
  caps =rep(0,n+1)
  for (i in 2:(n+1)){
    caps[i] = Two.factor.Hull.White.cap(rK=cap_rates[i],TK=Maturity[i],Delta=0.25,discount_factors,
                             gamma1,gamma2,sigma1,sigma2,rho)
    }
  return(caps)
}


