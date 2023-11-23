#' Function to calculate objective function in GMM minimization for CIR model.
#'
#' \deqn{f_1 = \frac{1}{N}\sum_{i=1}^N \left(r(i)- \alpha_1-\beta_1r(i-1)\right)}
#' \deqn{f_2 = \frac{1}{N}\sum_{i=1}^N [\left(r(i)- \alpha_1-\beta_1r(i-1)\right)^2 -r(i-1)\sigma^2]}
#' \deqn{ f_3 = \frac{1}{N}\sum_{i=1}^N \left[r(i)- \alpha_1-\beta_1r(i-1)\right]r(i-1)}
#' @param param is c(alpha1,beta1,sigma)
#' \deqn{\alpha_1  = \gamma\bar{r}\Delta}
#' \deqn{\beta_1 = 1- \gamma\Delta}
#' \deqn{\sigma = \sqrt{\alpha\Delta}}
#' @param X contains a sample path
#' @export
CIR.gmm = function(param,X){
  alpha1 = param[1]
  beta1= param[2]
  sigma = param[3]
  N = length(X)
  f1=0
  f2=0
  f3=0
  for (i in (2:N))
  {
    f1 = f1+X[i]-alpha1-beta1*X[i-1]
    f2 = f2 + (X[i]-alpha1-beta1*X[i-1])^2- X[i-1]*sigma^2
    f3 = f3 +(X[i]-alpha1-beta1*X[i-1])*X[i-1]
  }
  return(1/(N-1)*(f1^2+f2^2+f3^2))
}
