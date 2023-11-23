#'Price of a caplet v2
#' 
#' The difference between this function and Hull.White.caplet is that this function uses something similar to 
#' forward volatilities.
#'@param rK cap rate
#'@param T time of caplet payment
#'@param Delta time spacing.
#'@param ZTO price of a zero coupon bond that matures at time TO using two-factor Vasicek model
#'@param ZTB price of a zero coupon bond that matures at time TB using two-factor Vasicek model
#'@param SZTO is the appropriate Volatility
#'@export
Hull.White.capletv2 = function(rK,T,Delta=0.25,ZTO,ZTB,SZTO){
  TB =T
  TO = T-Delta
  K = 1/(1+rK*Delta)
  d1 = 1/SZTO * log(ZTB/(K*ZTO))+SZTO/2
  d2 = d1 - SZTO
  V0 = ZTO*pnorm(-d2)-ZTB/K*pnorm(-d1)
  return (V0)
}
