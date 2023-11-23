#'This function calculates log of  bond prices when the inputs are observed short term volatility, long term volatility
#'
#'@param t current time in years.
#'@param T maturity value; the yield is measured at time  for a zero-coupon bond that matures at T. 
#'@param Tau time in years of long term rate.
#'@param rt current short term rate.
#'@param rlTau current long term rate.
#'@param gamma1,gamma2,phi1,phi2, are \eqn{\gamma_1^*,\gamma_2^*,\phi_1} and \eqn{\phi_2},respectively.
#'@param short.term.vol,long.term.vol,correlation are volatility of short rate, volatility of long rate and correlation between long rate and short  rate.

#' and correlation between short and long term rates.
#'@export
Two.factor.Vasicek.yield = function(t,T,Tau,rt,rlTau,phi1,phi2,gamma1,gamma2,short.term.vol,long.term.vol,correlation){
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
  return ((-AtT+B1tT*phi1t+B2tT*phi2t)/(T-t))
}
