#' Function to compute minimizing quantity given in section 3.2
#' \eqn{J(\gamma^*,\bar{r}^*) = \sum_{i=1}^n (Z^{Vasicek}(r_0,0;T_i) - Z^{Data}(0,T_i))^2}
#'
#' This function will be used in ``nlm" and ``optim" as the objective function to be minimized.
#' It uses the function Vasicek.zcbp
#'
#
#'@param param is a list of parameters c(\eqn{\gamma,\bar{r},\sigma}).
#'@param r0 initial short rate default value is 0.0168
#'@param sigma if it is set to null minimization is  with respct to all three parameters \eqn{\gamma,\alpha} and \eqn{\sigma} otherwise
#' its minimiztion is with respect to \eqn{\gamma} and \eqn{\bar{r}} only.
#'@param bond.prices a list of observed zero coupon bond prices (or discount factors) for bond.maturities
#'@param bond.maturities a list bond maturities
#'
#'@export
Vasicek.J = function(param,r0=0.0168,sigma=NULL,bond.prices,bond.maturities){
  gamma = param[1]
  rbar = param[2]
  if (is.null(sigma))
    sigma= param[3]
  Bond.Prices = Vasicek.zcbp(r0=r0,t=0,T=bond.maturities,gamma,rbar,sigma)
 # return(sum((log(Bond.Prices)-log(bond.prices))^2))
  return(sum((Bond.Prices-bond.prices)^2))
}
