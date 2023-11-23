#' Yield of a zero coupon bond in Vasicek Model
#'
#' 
#' It Implements the following formulas.
#' \deqn{B(t;T) = \frac{1}{\gamma^*}\left( 1 - e^{-\gamma^* (T-t)}\right)}
#' \deqn{A(t;T) = (B(t;T) - (T-t))\left( \bar{r}^* - \frac{\sigma^2}{2(\gamma^*)^2} \right) - \frac{\sigma^2 B(t;T)^2}{4\gamma^*}}
#' \deqn{Z(r,t;T) = e^{A(t;T)-B(t;T) r}}
#' \deqn{Yield = -\frac{ ln(Z(r,t:T))}{T-t} }
#' default parameter values are those of Veronesi (2010) Example 15.1
#' @param t starting point of interest rate path.
#'
#' @param T ending time point.
#'
#'both t and T are given in years.
#'
#'
#'@param r0 is the starting value of the path.
#'
#'@param gamma is speed of mean reversion
#'
#'@param rbar is the long term mean.
#'
#'@param sigma is the instantaneous volatility.
#'
#'@export
#'
Vasicek.yield = function(r0=0.03,t=0,T=1,gamma=0.4653,rbar=0.0634,sigma=0.0221){
  # Text formulas (15.28)-(15.30)
  B = 1/gamma*(1-exp(-gamma*(T-t)))  # (15.29)
  A = (B-(T-t))*(rbar-sigma^2/(2*gamma^2))-sigma^2*B^2/(4*gamma) # (15.30)
  return((-A+B*r0)/(T-t))  # (15.29)
}
