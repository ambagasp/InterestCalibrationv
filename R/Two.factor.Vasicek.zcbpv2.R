#'Zero-coupon bond price under the two-factor Vasicek model.
#'
#'This function calculates log of  bond prices when the inputs are observed short term volatility, long term volatility
#' and correlation between short and long term rates.
#' @param t time of bond price calculation
#' @param T bond maturity time
#' @param rt short rate
#' @param Tau time of long rate.
#' @param rlTau long rate
#' @param phi1,phi2,gamma1,gamma2  are \eqn{\phi_1^*,\phi_2^*,\gamma_1^*,\gamma_2^*} and \eqn{\rho} respectively.\\
#' @param short.term.vol,long.term.vol,correlation are observed short term volatility, long term volatility
#' and correlation between short and long term rate
#'@export
Two.factor.Vasicek.zcbpv2 = function(t,T,Tau,rt,rlTau,phi1,phi2,gamma1,gamma2,short.term.vol,long.term.vol,correlation){
  est = Two.factor.Vasicek.Vols(gamma1,gamma2,Tau,short.term.vol,long.term.vol,correlation)
  sigma1= est[1]
  sigma2 = est[2]
  rho = est[3]
  B1Tau = Vasicek.B(gamma=gamma1,T=Tau)
  B2Tau = Vasicek.B(gamma=gamma2,T=Tau)
  ATau = Two.factor.Vasicek.A(phi1,phi2,gamma1,gamma2,sigma1,sigma2,rho,Tau)
  phi1t = (B2Tau*rt-Tau*rlTau-ATau)/(B2Tau-B1Tau)
  phi2t = (Tau*rlTau+ATau-rt*B1Tau)/(B2Tau-B1Tau)
  AtT = Two.factor.Vasicek.A(phi1,phi2,gamma1,gamma2,sigma1,sigma2,rho,(T-t))
  B1tT = Vasicek.B(gamma=gamma1,(T-t))
  B2tT = Vasicek.B(gamma=gamma2,(T-t))
  return (AtT-B1tT*phi1t-B2tT*phi2t)
}
