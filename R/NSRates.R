#' Spot rates (yield rate) with Nelson_Siegel method.
#'
#'It returns \eqn{r(0,t)=\beta_0 + \beta_1 \frac{ 1-\exp\left(-\frac{t}{\tau}\right) }{\frac{t}{\tau}} +\beta_2 \frac{1-\exp\left(-\frac{t}{\tau}\right)-\frac{t}{\tau}\exp\left(-\frac{t}{\tau}\right)}{\frac{t}{\tau}}}
#' The Yield curve express \eqn{\lambda = \frac{1}{\tau}}
#'@param coef the vector of parameters \eqn{\beta_0= coef[1], \beta_1=coef[2],\beta_2=coef[3],
#'\lambda=\frac{1}{\tau}=coef[4]}
#
#'@param maturity is the time to evaluate
#
#'
#'@export

NSRates = function(coef,maturity){
  out = coef[1]+ coef[2]*(1-exp(-maturity*coef[4]))/(maturity*coef[4])+
    coef[3]*((1-exp(-maturity*coef[4]))/(maturity*coef[4])-exp(-maturity*coef[4]))
  return(out)
}
