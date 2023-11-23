#' Price of a cap.
#' 
#' This is obtained by summing up individual caplet prices.
#' Calculate values of all the caplet that makes payment from 2*Delta to T2 and add them up
#' @param rK cap rate
#'@param TK is the time of last payment from the cap
#' @param Delta reciprocal of number of payments per year; it is assumed 4 times payments, Delta =0.25
#' @param discount_factors for times 0.25, 0.5,0.75...
#' @param sigma1 first factor  instantaneous  volatility 
#' @param sigma2 second factor instantaneous  volatility 
#' @param gamma1 first factor speed of mean reversion
#' @param gamma2 second factor speed of mean reversion
#' @param rho instantaneous correlation coefficient between two factors
#'@export
#'
Two.factor.Hull.White.cap = function(rK,TK,Delta=0.25,discount_factors,
                                     gamma1,gamma2,sigma1,sigma2,rho){
  no.caplets = TK/Delta -1

  sum( Two.factor.Hull.White.caplet(rK,T=seq(2*Delta,TK,Delta),Delta=0.25,ZTO=discount_factors[1:no.caplets],
                                    ZTB= discount_factors[2:(no.caplets+1)]
                                    ,gamma1,gamma2,sigma1,sigma2,rho))


  }
