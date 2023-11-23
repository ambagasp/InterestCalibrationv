#' Zero-coupon bond price under the two-factor Vasicek model.
#' 
#' The following function calculates zero-coupon bond prices when short rate rt at time t and 
#' long rate rlTau at Tau and other parameters
#' given, it first calculate the short rate factor  phi1t and long rate factor phi2t at time t.
#'@param t current time in years.
#'@param T maturity value
#'@param rt current value of the short rate.
#'@param rlTau current  value of the long term rate.
#'@param Tau time in years of long term rate.
#'@param gamma1,gamma2,sigma1,sigma2,rho,phi1,phi2  are \eqn{\gamma_1^*,\gamma_2^*,
#'\sigma_1,\sigma_2,\rho, \phi_1} and \eqn{phi_2} respectively.
#'@export
Two.factor.Vasicek.zcbp = function(t,T,Tau,rt,rlTau,phi1,phi2,gamma1,gamma2,sigma1,sigma2,rho){
  B1Tau = Vasicek.B(gamma=gamma1,T=Tau)
  B2Tau = Vasicek.B(gamma=gamma2,T=Tau)
  ATau = Two.factor.Vasicek.A(phi1,phi2,gamma1,gamma2,sigma1,sigma2,rho,Tau)
  phi1t = (B2Tau*rt-Tau*rlTau-ATau)/(B2Tau-B1Tau)
  phi2t = (Tau*rlTau+ATau-rt*B1Tau)/(B2Tau-B1Tau)
  AtT = Two.factor.Vasicek.A(phi1,phi2,gamma1,gamma2,sigma1,sigma2,rho,(T-t))
  B1tT = Vasicek.B(gamma=gamma1,T=(T-t))
  B2tT = Vasicek.B(gamma=gamma2,T=(T-t))
  return (exp(AtT-B1tT*phi1t-B2tT*phi2t))
}
