#' This function calculates sigma1, sigma 2 and rho when volatility of short, volatility of long rate and
#' correlation between long and short in equations are given.


#'@param gamma1,gamma2 are \eqn{\gamma_1^*} and \eqn{\gamma_2^*},respectively.
#'@param short.term.vol,long.term.vol,correlation are volatility of short rate, volatility of long rate and correlation between long rate and short  rate.
#'@param Tau time in years of long term rate.
#'@export
Two.factor.Vasicek.Vols = function(gamma1=0.8269,
                                   gamma2=-0.0288,Tau=5,short.term.vol=0.0221,
                                   long.term.vol=0.0125,correlation=0.4713){
  B1Tau = Vasicek.B(gamma=gamma1,T=Tau)/Tau
  B2Tau = Vasicek.B(gamma=gamma2,T=Tau)/Tau
  if (correlation==0) {
    A = matrix(c(1,1,B1Tau^2,B2Tau^2),nrow=2,ncol=2,byrow=TRUE)
    B = matrix(c(short.term.vol^2,long.term.vol^2),nrow=2,ncol=1)
    est= solve(A,B)
    paras = c(est[1]^0.5,est[2]^0.5,0)
    return(paras)
  }
  else {
    cova = short.term.vol*long.term.vol*correlation
    A = matrix(c(1,1,2,B1Tau^2,B2Tau^2,2*B1Tau*B2Tau,B1Tau,B2Tau,(B1Tau+B2Tau)),nrow=3,ncol=3,byrow=TRUE)
    B = matrix(c(short.term.vol^2,long.term.vol^2,cova),nrow=3,ncol=1)
    est= solve(A,B)
    paras = c(est[1]^0.5,est[2]^0.5,est[3]/(est[1]*est[2])^0.5)
    return(paras)
  }
}
