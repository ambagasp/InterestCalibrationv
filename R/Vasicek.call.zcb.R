#' Price of a call option on zero coupon bond with Vasicek model
#'
#' It implements European call price formula 
#' \deqn{V(r_0,0) = Z(r_0,0;T_B){N}(d_1) -KZ(r_0,0;T_O){N}(d_2)}
#' \deqn{d_1 = \frac{1}{S_Z(T_O;T_B)} \log \left(\frac{Z(r_0,0;T_B)}{KZ(r_0,0;T_O)}\right)+\frac{S_Z(T_O;T_B)}{2}}
#' \deqn{d_2 = d_1 - S_Z(T_O;T_B)}
#' \deqn{S_Z(T_O;T_B) = B(T_O;T_B) \times \sqrt{\frac{\sigma^2}{2\gamma^*}(1 - e^{-2\gamma^* T_O})}}

#' @param r0 short  rate at time 0
#' @param TO option maturity time
#' @param TB zero coupon bond maturity time
#' @param K option strike price as a percentage of face value
#'@param gamma is speed of mean reversion
#'@param rbar is the long term mean.
#'@param sigma is the instantaneous volatility.
#' default \eqn{\gamma,\bar{r}} and \eqn{\sigma} are the values in example 15.1
#' @export
Vasicek.call.zcb = function(r0=0.03,TO=0.5,TB=1,K=0.98,gamma=0.4653,rbar=0.0634,sigma=0.0221){
  SZ = 1/gamma*(1-exp(-gamma*(TB-TO)))*(sigma^2*(1-exp(-2*gamma*TO))/(2*gamma))^0.5
  ZCBTB= Vasicek.zcbp(r0,t=0,T=TB,gamma,rbar,sigma)
  ZCBTO= Vasicek.zcbp(r0,t=0,T=TO,gamma,rbar,sigma)
  d1 = 1/SZ*log(ZCBTB/(K*ZCBTO))+SZ/2
  d2 =d1 -SZ
  return((ZCBTB*pnorm(d1)-K*ZCBTO*pnorm(d2)))
}
