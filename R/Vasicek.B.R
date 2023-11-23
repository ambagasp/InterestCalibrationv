#' B(0;T) in the Vasicek bond pricing formula.
#' 
#' It returns \deqn{B(0,T)=\frac{1-\exp(-\gamma T)}{\gamma},~\text{if}~~ \gamma \ne =0}
#' \deqn{B(0,T)= T~\text{if}~~ \gamma  =0}
#' 
#'@param gamma is the mean reversion speed
#'@param T is the time to maturity
#'@export
Vasicek.B = function(gamma,T){
  if (gamma==0) {
    return(T)
  }
  else {
    return((1-exp(-gamma*T))/gamma)
  }
}
