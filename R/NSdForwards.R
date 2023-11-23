#'First derivative of Nelson-Siegel Forward rates with respect to time.
#'
#' It returns \eqn{\frac{\partial}{\partial t} f(0,t)= -\frac{\beta_1}{\tau}\exp\left(-\frac{t}{\tau}\right)
#'+\frac{\beta_2}{\tau}\exp\left(-\frac{t}{\tau}\right)-
#'  \frac{\beta_2t}{\tau^2}\exp\left(-\frac{t}{\tau}\right)}
#'The package Yield curve express \eqn{\lambda = \frac{1}{\tau}}
#'@param coef the vector of parameters \eqn{\beta_0= coef[1], \beta_1=coef[2],\beta_2=coef[3],
#'\lambda=\frac{1}{\tau}=coef[4]}
#
#'@param maturity is the time to evaluate
#
#'
#'@export


NSdForwards = function(coef,maturity){
  out = -coef[2]*coef[4]*exp(-maturity*coef[4])+
    coef[3]*coef[4]*exp(-maturity*coef[4]) -
    coef[3]*maturity*coef[4]^2*exp(-maturity*coef[4])
  return(out)
}
