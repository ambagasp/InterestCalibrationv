#' sum of squared difference between market prices and theoretical prices of zeros in CIR.
#'
#' Function to compute the sum of squared of differences between the CIR theoretical bond price
#' and the market price of zero-coupon bonds.
#'This function will be used in ``nlm" and ``optim" as the objective function to be minimized.
#' It uses the function CIR.zcbp
#'
#
#'@param param is a list of parameters c(\eqn{\gamma,\bar{r},\alpha}).
#'@param r0 initial short rate default value is 0.0168
#'@param alpha if it is set to null minimization is  with respect to all three 
#' parameters \eqn{\gamma,\bar{r}} and \eqn{\alpha} otherwise
#' its minimiztion is with respect to \eqn{\gamma} and \eqn{\bar{r}} only.
#'@param bond.prices a list of observed zero coupon bond prices (or discount factors) 
#'for bond.maturities
#'@param bond.maturities a list bond maturities
#'
#'@export

CIR.J = function(param,r0=0.0168,alpha=NULL, bond.prices,bond.maturities){
  gamma = param[1]
  rbar = param[2]
  if (is.null(alpha))
    alpha = param[3]
  Bond.Prices = CIR.zcbp(r0=r0,t=0,T=bond.maturities,gamma,rbar,
                         alpha)
  return(sum((Bond.Prices-bond.prices)^2))
}
