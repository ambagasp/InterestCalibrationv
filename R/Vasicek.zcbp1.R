#' zero coupon bond pricing formula
#'
#' This is similar to Vasicek.zcbp but returning  additional intermediate values
#'
#' default parameter values are those of Veronesi (2010) Example 15.1
#' @param t starting point of interest rate path.
#'
#' @param T ending time point.
#'
#'both t and T are given in years.
#'
#'
#'@param r0 is the starting value of the path.
#'
#'@param gamma is speed of mean reversion
#'
#'@param rbar is the long term mean.
#'
#'@param sigma is the instantaneous volatility.
#'@export

Vasicek.zcbp1 = function(r0=0.03,t=0,T=1,gamma=0.4653,rbar=0.0634,sigma=0.0221){
  B = 1/gamma*(1-exp(-gamma*(T-t)))  # (15.29)
  A = (B-(T-t))*(rbar-sigma^2/(2*gamma^2))-sigma^2*B^2/(4*gamma) # (15.30)
  Price = exp(A-B*r0)  # (15.29)
  Mat= matrix(c(T-t,A,B,Price),ncol=4,byrow=FALSE)
  colnames(Mat)=c("T-t","A(T-t)","B(T-t)","Price(T-t)")
  return(Mat)
}
