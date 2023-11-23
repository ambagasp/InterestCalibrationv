#' Calculates caplet price with the  two-factor Hull-White model.
#' 
#' 
#' Caplet price is calculated as the price of a put option on a zero-coupon bond.
#'@param rK cap rate
#'@param T time of caplet payment.
#'@param Delta time in years between two caplet payments. 
#'@param ZTO price of a zero coupon bond that matures at time TO using two-factor Vasicek model
#'@param ZTB price of a zero coupon bond that matures at time TB using two-factor Vasicek model
#'@param gamma1,gamma2,sigma1,sigma2,rho and correlation are \eqn{\gamma_1^*,\gamma_2^*,
#'\sigma_1,\sigma_2} and \eqn{\rho} respectively.
#'@export
Two.factor.Hull.White.caplet = function(rK,T,Delta=0.25,ZTO,ZTB,gamma1,gamma2,sigma1,sigma2,rho){
  TB =T
  TO = T-Delta
  K = 1/(1+rK*Delta)
  B1TOTB = Vasicek.B(gamma=gamma1,T=(TB-TO))
  B2TOTB = Vasicek.B(gamma=gamma2,T=(TB-TO))
  SZTO2 = B1TOTB^2 * sigma1^2*Vasicek.B(gamma=2*gamma1,T=TO) +
    B2TOTB^2 * sigma2^2*Vasicek.B(gamma=2*gamma2,T=TO) +
    B1TOTB*B2TOTB* sigma1*sigma2*rho*Vasicek.B(gamma=(gamma1+gamma2),T=TO)
  SZTO = SZTO2^0.5
  d1 = 1/SZTO * log(ZTB/(K*ZTO))+SZTO/2
  d2 = d1 - SZTO
  V0 = ZTO*pnorm(-d2)-ZTB/K*pnorm(-d1)
  return (V0)
}
