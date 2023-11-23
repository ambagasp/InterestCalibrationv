#' Function to evaluate a polynomial or its derivative at a given point.
#'
#'It returns \eqn{ p(x) = a_0 + a_1 x +a_2x + \ldots+a_nx^n}
#'@param x the point which polynomial has to be evaluated.
#'
#'@param coeffs coefficient vector in the order \eqn{a_0,a_1,\ldots,a_n}
#'
#'@param deriv indicates which derivative it needs, calculate up to second derivative
#'
#'@export
polyfit = function(x,coeffs,deriv=0){
  n = length(coeffs)
  if (deriv==0)
    out= sum(x^c(0:(n-1))*coeffs)
  else if (deriv==1)
    out = sum(x^c(0:(n-2))*coeffs[2:n]*c(1:(n-1)))
  else if (deriv==2)
    out = sum(x^c(0:(n-3))*coeffs[3:n]*c(1:(n-2))*c(2:(n-1)))
  else
    stop("error")
  return(out)
}
