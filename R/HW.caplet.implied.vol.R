#' This function used in extracting  implied volatility of a caplet.
#' 
#' Its is very similar to Hull.White.caplet.
#' 
#' @param sigmaf is the implied vol
#' @param price of a caplet
#' @param rK cap rate applicable to the caplet
#' @param T caplet payment tim
#' @param Delta the cap rate applicaable is decided at time T-Delta
#' @param ZTO discount factor applicable to time T-Delta
#' @param ZTB discount faactor applcable to time T
#'@export
HW.caplet.implied.vol = function(sigmaf, price=0.0786/100, 
                                 rK= 0.02442,T=0.75,Delta=0.25,
                                 ZTO=0.988510,ZTB=0.981899){
  
  TB =T
  TO = T-Delta
  K = 1/(1+rK*Delta)
  d1 = 1/sigmaf * log(ZTB/(K*ZTO))+sigmaf/2
  d2 = d1 - sigmaf
  V0 = ZTO*pnorm(-d2)-ZTB/K*pnorm(-d1)-price
  return(V0)
}