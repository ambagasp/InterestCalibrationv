#'Forward rates with Nelson-Siegel Model
#'
#' It returns \eqn{f(0,t) = \beta_0 + \beta_1 \exp\left(-\frac{t}{\tau}\right )+
#' \frac{\beta_2 t}{\tau} \exp\left(-\frac{t}{\tau}\right)}
#'The package Yield curve express \eqn{\lambda = \frac{1}{\tau}}
#'@param coef the vector of parameters \eqn{\beta_0= coef[1], \beta_1=coef[2],\beta_2=coef[3],
#'\lambda=\frac{1}{\tau}=coef[4]}
#
#'@param maturity is the time to evaluate
#
#'
#'@export

NSForwards = function(coef,maturity){
  out = coef[1]+ coef[2]*exp(-maturity*coef[4])+
    coef[3]*maturity*coef[4]*exp(-maturity*coef[4])
  return(out)
}
