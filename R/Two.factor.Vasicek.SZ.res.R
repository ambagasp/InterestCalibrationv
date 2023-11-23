#' Calculate residuals for forward volatility two-factor in Hull-White models.
#' @param paras contains the parameters that need to be estimated
#' gamma1=paras[1]
#' gamma2=paras[2]
#'sigma1 = paras[3]
#' sigma2 = paras[4]
#' rho = paras[5]
#' @param  forward_vol.dat contains maturity and Volatility divided by thee time it calculated
#'@export
Two.factor.Vasicek.SZ.res = function(paras,forward_vol.dat){
  gamma1=paras[1]
  gamma2=paras[2]
  sigma1 = paras[3]
  sigma2 = paras[4]
  rho = paras[5]
  Delta = 0.25
  TO = forward_vol.dat$Maturity-0.25
  B1 = Vasicek.B(gamma=gamma1,T=Delta)
  B2 = Vasicek.B(gamma=gamma2,T=Delta)
  B1G1T = Vasicek.B(gamma=(2*gamma1),T=TO)
  B2G2T = Vasicek.B(gamma=(2*gamma2),T=TO)
  BG1G2T = Vasicek.B(gamma=(gamma1+gamma2),T=TO)
  Res = ((B1*sigma1)^2*B1G1T+(B2*sigma2)^2*B2G2T+
           B1*B2*BG1G2T*sigma1*sigma2*rho)/TO-
    forward_vol.dat$`Forward Vol.sq`
  return(Res)
}