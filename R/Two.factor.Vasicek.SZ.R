#' Volatility calculation.
#' 
#' This volatility is used in the call option on bonds when underlying model is the
#' two-factor Vasicek.
#'@param TO time to option maturity
#'@param TB time to maturity of the underlying zero-coupon bond; when option expires underlying bond will have TB-TO remaining
#'
#'@param gamma1,gamma2,sigma1,sigma2,rho  are \eqn{\gamma_1^*,\gamma_2^*,
#'\sigma_1,\sigma_2} and \eqn{\rho} respectively.
#'@export
Two.factor.Vasicek.SZ = function(TO,TB,gamma1,gamma2,sigma1,sigma2,rho){
  B1TOTB = Vasicek.B(gamma=gamma1,T=(TB-TO))
  B2TOTB = Vasicek.B(gamma=gamma2,T=(TB-TO))
  SZTO2 = B1TOTB^2 * sigma1^2*Vasicek.B(gamma=2*gamma1,T=TO) +
    B2TOTB^2 * sigma2^2*Vasicek.B(gamma=2*gamma2,T=TO) +
    B1TOTB*B2TOTB* sigma1*sigma2*rho*Vasicek.B(gamma=(gamma1+gamma2),T=TO)
  SZTO = SZTO2^0.5
  return(SZTO)
}