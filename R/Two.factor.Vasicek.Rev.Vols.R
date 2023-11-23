#'Calculating long and short term volatility.
#'
#' Function to calculate short term volatility, long term volatility and correlation of
#' the two-factor Vasicek model
#' It implements some formulas in the Section 3.6 of the article.
#'@param gamma1,gamma2,sigma1,sigma2,rho and correlation are \eqn{\gamma_1^*,\gamma_2^*,\sigma_1,\sigma_2} and \eqn{\rho} respectively.
#'@param Tau time of long rate
#'@export
Two.factor.Vasicek.Rev.Vols = function(gamma1=1.7227,
                                       gamma2=0.0434,sigma1=0.0222,sigma2=0.0147,
                                       rho=-0.3969,Tau=5){
  B1Tau = Vasicek.B(gamma=gamma1,T=Tau)/Tau
  B2Tau = Vasicek.B(gamma=gamma2,T=Tau)/Tau
  sigmat = (sigma1^2+sigma2^2+2*sigma1*sigma2*rho)^0.5
  sigmatu = ((sigma1*B1Tau)^2+(sigma2*B2Tau)^2 + 
               2*sigma1*sigma2*rho*B1Tau*B2Tau)^0.5
  correlation = (sigma1^2*B1Tau+sigma2^2*B2Tau+
                   (B1Tau+B2Tau)*sigma1*sigma2*rho)/(sigmat*sigmatu)
  return(c(sigmat,sigmatu,correlation))
}