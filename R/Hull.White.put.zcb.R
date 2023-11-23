#' Price of put option on a zero-coupon bond under Hull-White model.
#'
#'
#'@param TO option maturity
#'@param TB maturity of underlying bond
#'@param K strike price of the option
#'@param gamma mean reversion rate of Hull-White
#'@param sigma instantaneous Hull-White volatility
#'@param ZCBTB bond price of a zero coupon that matures at time TB
#'@param ZCBTO bond price of a zero coupon that matures at time TO
#'@export
Hull.White.put.zcb = function(TO=1,TB=5,K=0.8,gamma=0.019,sigma=0.0196,ZCBTB=0.7978,ZCBTO=0.9790){
  #BTOTB = 1/gamma*(1-exp(-gamma*(TB-TO)))
  #BTOTB = Vasicek.B(gamma,T=(TB-TO))
  #SZ = (BTOTB^2*sigma^2/(2*gamma)*(1-exp(-2*gamma*TO)))^0.5
  SZ = Vasicek.B(gamma=gamma,T=(TB-TO))/sigma*(Vasicek.B(gamma=2*gamma,T=TO))^0.5
  d1 = 1/SZ*log(ZCBTB/(K*ZCBTO))+SZ/2
  d2 =d1 -SZ
  return((-ZCBTB*pnorm(-d1)+K*ZCBTO*pnorm(-d2)))
}
