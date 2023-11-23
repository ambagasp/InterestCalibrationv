#'Price of a cap v2
#'
#' The difference between this function and Hull.White.cap is that this function uses something similar to 
#' forward volatilities.
#' 
#' @param rK cap rate
#' @param TK maturity of Cap
#' @param Delta is normally 0.25
#' @param discount_factors are discount factors applicable to 0.25,0.5,.. TK
#' @param sigmaf Hull-White forward volatility applicable to each point in Maturity
#'@export
#'
Hull.White.capv2 = function(rK=0.05,TK=5,Delta=0.25,discount_factors,sigmaf){
  
  no.caplets = TK/Delta -1
  
  sum(Hull.White.capletv2(rK=rK,T=seq(2*Delta,TK,Delta),Delta=Delta,
                          ZTO=discount_factors[1:no.caplets],
                          ZTB=discount_factors[2:(no.caplets+1)],SZTO=sigmaf))
}
