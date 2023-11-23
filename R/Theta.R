#' Theta of Hull-White model with Nelson-Siegel curve
#' 
#' It calculates \eqn{\theta_T = \frac{\partial f(0,T)}{\partial T} +
#' \gamma^* f(0,T)+\frac{\sigma^2}{2\gamma^*}(1-\exp(-2\gamma^*T)}
#' 
#'@param coef the vector of parameters \eqn{\beta_0= coef[1], \beta_1=coef[2],\beta_2=coef[3],
#'\lambda=\frac{1}{\tau}=coef[4]}
#
#'@param sigma is \eqn{\sigma}
#'@param gamma is \eqn{\gamma}
#'@param maturity is the time to evaluate
#
#'
#' 
#'@export
Theta = function(coef,sigma=0.0221, gamma=0.19, maturity){
  out = NSdForwards(coef=coef,maturity=maturity)/100+
    gamma*NSForwards(coef=coef,maturity=maturity)/100+
    sigma^2/(2*gamma)*(1-exp(-2*gamma*maturity))
  return(out)
}