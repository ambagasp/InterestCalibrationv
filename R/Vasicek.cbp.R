#' Vasicek formula for coupon bonds

#' @param TO the point of valuation
#'
#'
#'@param rTO is the short rate at time t
#'
#'@param gamma is the mean reversion speed
#'
#'@param rbar is the long term mean
#'
#'@param sigma is the instantaneous volatility.
#'
#'@param cash_flow_timing vector containing cash flow timing in years
#'@param cash_flow  vector containing cash flows at cash_flow_timing
#'@export
Vasicek.cbp= function(rTO=0.0342,gamma=0.4653,rbar=0.0634,sigma=0.0221,TO=1,
                      cash_flow_timing=c(3:10)/2, cash_flow=c(rep(2.5,7),102.5)){
  B = 1/gamma*(1-exp(-gamma*(cash_flow_timing-TO)))
  A = (B-(cash_flow_timing-TO))*(rbar-sigma^2/(2*gamma^2))-sigma^2*B^2/(4*gamma)
  ZTO = sum(exp(A-B*rTO)*cash_flow)
  return(ZTO)
}
