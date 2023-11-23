#' Value of put option value on a zero coupon bond.
#' 
#' This Calculates the value of a put on a zero coupon bond when underlying model is twwo-factor Vasicek.
#' 
#' 
#'@param TO time to option maturity
#'@param TB time to maturity of the underlying zero-coupon bond; when option expires underlying bond will have TB-TO remaining
#'@param K strike price
#'@param ZTO price of a zero coupon bond that maturues at time TO using two-factor Vasicek model
#'@param ZTB price of a zero coupon bond that matures at time TB using two-factor Vasicek model
#'@param phi1,phi2,gamma1,gamma2,sigma1,sigma2,rho are \eqn{\phi_1^*,\phi_2^*,\gamma_1^*,\gamma_2^*,
#'\sigma_1,\sigma_2} and \eqn{\rho} respectively.
#'@export
Two.factor.Vasicek.put.zcb = function(TO,TB,K,ZTO,ZTB,phi1,phi2,gamma1,gamma2,sigma1,sigma2,rho){
  #B1TOTB = 1/gamma1*(1-exp(-gamma1*(TB-TO)))
  #B2TOTB = 1/gamma2*(1-exp(-gamma2*(TB-TO)))
  B1TOTB = Vasicek.B(gamma=gamma1,T=(TB-TO))
  B2TOTB = Vasicek.B(gamma=gamma2,T=(TB-TO))
  SZTO2 = B1TOTB^2 * sigma1^2*Vasicek.B(gamma=2*gamma1,T=TO) +
    B2TOTB^2 * sigma2^2*Vasicek.B(gamma=2*gamma2,T=TO) +
    B1TOTB*B2TOTB* sigma1*sigma2*rho*Vasicek.B(gamma=(gamma1+gamma2),T=TO)
  SZTO = SZTO2^0.5
  d1 = 1/SZTO * log(ZTB/(K*ZTO))+SZTO/2
  d2 = d1 - SZTO
  V0 = -ZTB*pnorm(-d1)+K*ZTO*pnorm(-d2)
  return (V0)
}
