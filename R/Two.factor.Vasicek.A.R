#' A in the tow-factor Vasicek bond pricing formula.
#' 
#' This function calculates A as described in equations in the Section 3.6 of the article.
#' 
#' 
#' It handles the situation where \eqn{\gamma_1^*=0} and or \eqn{\gamma_2^*=0}
#' 
#'@param phi1 long term average for factor 1.
#'@param phi2 long term average for factor 2.
#' @param sigma1 first factor  instantaneous  volatility 
#' @param sigma2 second factor instantaneous  volatility 
#' @param gamma1 first factor speed of mean reversion
#' @param gamma2 second factor speed of mean reversion
#' @param rho instantaneous correlation coefficient between two factors
#'@param T maturity date.
#'
#'@export
Two.factor.Vasicek.A = function(phi1,phi2,gamma1,gamma2,sigma1,sigma2,rho,T){
  B1T = Vasicek.B(gamma=gamma1,T)
  B2T = Vasicek.B(gamma=gamma2,T)
  B3T = Vasicek.B(gamma=(gamma1+gamma2),T)
  if (gamma1 !=0  & gamma2 !=0){
    AtT = (B1T-T)*(phi1-sigma1^2/(2*gamma1^2)- sigma1*sigma2*rho/(gamma1*gamma2))-sigma1^2/(4*gamma1)*B1T^2 +
      (B2T-T)*(phi2-sigma2^2/(2*gamma2^2)- sigma1*sigma2*rho/(gamma1*gamma2))-sigma2^2/(4*gamma2)*B2T^2 +
      (B3T-T)*sigma1*sigma2*rho/(gamma1*gamma2)
  }
  else if (gamma1 ==0 & gamma2 !=0){
    AtT = phi2*(B2T-T)+sigma1^2*T^3/6-sigma2^2/2*(1/gamma2^2*(B2T-T)+B2T^2/(2*gamma2))+
      rho*sigma1*sigma2*(T^2/2+(exp(-gamma2*T)*(gamma2*T+1)-1)/gamma2^2)
  }
  else if (gamma2 ==0 & gamma1 !=0){
    AtT = phi1*(B1T-T)+sigma2^2*T^3/6-sigma1^2/2*(1/gamma1^2*(B1T-T)+B1T^2/(2*gamma1))+
      rho*sigma1*sigma2*(T^2/2+(exp(-gamma1*T)*(gamma1*T+1)-1)/gamma1^2)
  }
  else {
    AtT = (sigma1^2+sigma2^2+2*rho*sigma1*sigma2)*T^3/6
  }
  return(AtT)
}
