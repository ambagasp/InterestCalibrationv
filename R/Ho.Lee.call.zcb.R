#'price of  a call option on a zero-coupon bond under Ho-Lee model.
#'@param TO option maturity
#'@param TB maturity of underlying bond
#'@param K strike price of the option
#'@param sigma instantaneous Ho-Lee volatility
#'@param ZCBTB bond price of a zero coupon that matures at time TB
#'@param ZCBTO bond price of a zero coupon that matures at time TO
#'@export
Ho.Lee.call.zcb = function(TO=1,TB=5,K=0.8,sigma=0.0221,ZCBTB=0.7978,ZCBTO=0.9790){
  SZ = (TB-TO)*sigma*TO^0.5
  d1 = 1/SZ*log(ZCBTB/(K*ZCBTO))+SZ/2
  d2 =d1 -SZ
  return((ZCBTB*pnorm(d1)-K*ZCBTO*pnorm(d2)))
}
