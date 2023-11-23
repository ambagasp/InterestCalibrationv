#' Yield of a zero coupon bond in  CIR model
#'
#' It implements the formulas in the Section 3.5 of the article.
#' \deqn{ B(t;T) = \frac{2\left( e^{\psi_1 (T-t)} - 1\right)}{(\gamma^* + \psi_1)\left(e^{\psi_1(T-t)}-1\right) + 2\psi_1}}
#' \deqn{ A(t;T) = 2\frac{\bar{r}^* \gamma^*}{\alpha}\log \left( \frac{2\psi_1  e^{(\psi_1+\gamma^*)\frac{(T-t)}{2}} }{(\gamma^* + \psi_1)\left(e^{\psi_1(T-t)}-1\right) + 2\psi_1} \right)}
#' \deqn{\psi_1 = \sqrt{(\gamma^*)^2+2\alpha}  }
#' \deqn{Z(r,t;T) = e^{A(t;T)-B(t;T)\times r}}
#' \deqn{Yield = -\frac{ ln(Z(r,t:T))}{T-t} }
#'
#'
#' @param t time where zero coupon bond price is evaluated in years.
#'
#' @param T maturity time on years. 
#'
#'@param r0 is the short rate at time t
#'
#'@param gamma is the mean reversion speed
#'
#'@param rbar is the long term mean
#'
#'@param alpha rbar*gamma> alpha/2 not to have negative rates
#'
#'@export
#'
CIR.yield = function(r0=0.02,t=0,T=1,gamma=0.3807,rbar=0.072,alpha=0.0548){
  psi1 = (gamma^2+2*alpha)^0.5
  Denom = (gamma+psi1)*(exp(psi1*(T-t))-1)+ 2*psi1
  B = 2*(exp(psi1*(T-t))-1)/Denom
  A = 2*rbar*gamma/alpha* log((2*psi1*exp((psi1+gamma)*(T-t)/2))/Denom)
  return((-A+B*r0)/(T-t))
}
