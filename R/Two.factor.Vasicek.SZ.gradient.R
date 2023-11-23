#' Calculate the Jacobian of residual  function.
#' 
#' The residual function is  for forward volatility of two-factor in Hull-White models.
#' @param paras contains the parameters that need to be estimated
#' gamma1=paras[1],
#' gamma2=paras[2],
#'sigma1 = paras[3],
#' sigma2 = paras[4],
#' rho = paras[5].
#' @param  forward_vol.dat contains maturity and Volatility divided by thee time it calculated
#'@export
Two.factor.Vasicek.SZ.gradient = function(paras,forward_vol.dat){
  gamma1=paras[1]
  gamma2=paras[2]
  sigma1 = paras[3]
  sigma2 = paras[4]
  rho = paras[5]
  Delta = 0.25
  TO = forward_vol.dat$Maturity-0.25
  Nsize = length(fvol.dat$Maturity)
  B1 = Vasicek.B(gamma=gamma1,T=Delta)
  B2 = Vasicek.B(gamma=gamma2,T=Delta)
  B1G1T = Vasicek.B(gamma=(2*gamma1),T=TO)
  B2G2T = Vasicek.B(gamma=(2*gamma2),T=TO)
  BG1G2T = Vasicek.B(gamma=(gamma1+gamma2),T=TO)
  B1D = Vasicek.DB(gamma=gamma1,T=Delta)
  B2D = Vasicek.DB(gamma=gamma2,T=Delta)
  B1G1TD = Vasicek.DB(gamma=(2*gamma1),T=TO)*2
  B2G2TD = Vasicek.DB(gamma=(2*gamma2),T=TO)*2
  BG1G2TD = Vasicek.DB(gamma=(gamma1+gamma2),T=TO)
  Jacob = matrix(0.0,Nsize,5)
  Jacob[1:Nsize,1] = (2*B1*B1D*B1G1T + B1^2*B1G1TD)*sigma1^2+
    (B1D*B2*BG1G2T+B1*B2*BG1G2TD)*sigma1*sigma2*rho
  Jacob[1:Nsize,2] = (2*B2*B2D*B2G2T + B2^2*B2G2TD)*sigma2^2+
    (B2D*B1*BG1G2T+B1*B2*BG1G2TD)*sigma1*sigma2*rho
  Jacob[1:Nsize,3] = B1^2*B1G1T*2*sigma1+ B1*B2*BG1G2T*sigma2*rho
  Jacob[1:Nsize,4] = B2^2*B2G2T*2*sigma2+ B1*B2*BG1G2T*sigma1*rho
  Jacob[1:Nsize,5] = B1*B2*BG1G2T*sigma1*sigma2
  Jacob = Jacob/TO
  attr(Jacob,"gradient") =Jacob
  return(Jacob)
}