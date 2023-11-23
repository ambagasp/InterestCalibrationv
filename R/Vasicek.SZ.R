#' This is the volatility used in Vasicek, Ho-Lee and Hull-White option pricing formulas.
#'
#' It implements the following formula which can be used in many routines.
#' \deqn{S_Z(T_O;T_B) = B(T_O;T_B) \times \sqrt{\frac{\sigma^2}{2\gamma^*}(1 - e^{-2\gamma^* T_O})}}
#' \deqn{S_Z(T_O;T_B) = \sigma (T_B-T_O)\sqrt{T_O}~~\text{if} \gamma^*= 0}
#' 
#' @param TO option maturity time
#' @param TB zero coupon bond maturity time
#'@param gamma is speed of mean reversion
#'@param sigma is the instantaneous volatility.
#' default \eqn{\gamma,\bar{r}} and \eqn{\sigma} are the values in example 15.1
#' @export
Vasicek.SZ= function(TO=0.5,TB=1,gamma=0.4653,sigma=0.0221){
  SZ = sigma*Vasicek.B(gamma=gamma,T=(TB-TO))*(Vasicek.B(gamma=(2*gamma),T=TO))^0.5
  return(SZ)
}