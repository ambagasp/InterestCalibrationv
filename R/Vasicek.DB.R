#' Derivative of B (Vasicek bond pricing formula B)
#' 
#' 
#' It returns derivative of \eqn{B=(0,T)=\frac{1-\exp(-\gamma T)}{\gamma}} with respect to \eqn{\gamma}
#' the derivative at  \eqn{gamma=0} calculated as the limit.
#'@param gamma is the mean reversion speed
#'@param T is the time to maturity
#'@export
Vasicek.DB = function(gamma,T){
  if (abs(gamma)<1e-8) {
    return(-(T^2)/2)
  }
  else {
    return(-(1-exp(-gamma*T))/gamma^2+T*exp(-gamma*T)/gamma)
  }
}