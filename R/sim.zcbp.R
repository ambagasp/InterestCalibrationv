#' Zero coupon bond prices using simulation
#'
#'
#' This function takes multiple interest rate paths given as columns of the time series matrix X
#' Integral is approximated by Riemann-sums
#' @param X is a multivariate time series object. Each column is an interest rate path
#' @export
sim.zcbp = function(X){
  n = length(X[,1])-1
  dt = deltat(X)
  return(mean(exp(-colSums(X[1:n,])*dt)))
}
