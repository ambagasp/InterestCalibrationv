#' Log likelihood function of CIR model.
#'
#' It takes one interest rate path and calculate log likelihood function.
#' 
#' It uses the function CIR.log.density.
#'
#' @param param is the list  c(gamma, rbar,alpha)
#' @param X contains an interest rate path a time series object
#' @export
CIR.log.lik = function(param,X){
  # gamma = param[1]
  # rbar = param[2]
  # alpha = param[3]
  N =length(X)-1
  Delta = deltat(X)
  return(-sum(CIR.log.density(x=X[2:(N+1)],Delta,x0=X[1:N],gamma=param[1],rbar=param[2],alpha=param[3])))
}
