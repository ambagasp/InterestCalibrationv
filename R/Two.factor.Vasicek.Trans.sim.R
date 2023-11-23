#' Two-factor Vasicek path simulation using Transition density  approach.
#'
#' This function simulate M  interest rate paths from the Vasicek model using transition density  approach.
#'
#' @param t0 starting point of interest rate path.
#'
#' @param T ending time point.
#'
#'both t0 and T are given in years.
#'
#'@param Delta is the time step; default value being 1 day (1/252 in years).
#'@param r0 is the starting value of the path (starting value of short rate).
#'@param r05 starting value of the long term rate.
#'@param Tau time in years of long term rate.
#'@param gamma1,gamma2,sigma1,sigma2,rho,phi1,phi2  are \eqn{\gamma_1^*,\gamma_2^*,
#'\sigma_1,\sigma_2,\rho, \phi_1} and \eqn{phi_2} respectively.
#'@param M is the required number of interest rate paths
#'
#'Default values of r0, gamma, rbar and alpha are the ones given in Veronesi(2010) Table 15.3 real world parameters.
#'@export
Two.factor.Vasicek.Trans.sim =function(t0=0,T=10,Delta=1/252,r0=0.0437,r05=0.0454,Tau=5,
                                       gamma1=1.7227,gamma2=0.04343,
                                       phi1 =0.2993,phi2=0,
                                       sigma1=0.0222,sigma2=0.0147,
                                       rho =-0.3969,M=10){
  N = ceiling((T-t0)/Delta)
  rhodelta = rho*2*(1-exp(-(gamma1+gamma2)*Delta))/(gamma1+gamma2)*
             (gamma1*gamma2/(1-exp(-2*gamma1*Delta))/(1-exp(-2*gamma2*Delta)))^0.5
  sigmast1 = sigma1*((1-exp(-2*gamma1*Delta))/(2*gamma1))^0.5
  sigmast2 = sigma2*((1-exp(-2*gamma2*Delta))/(2*gamma2))^0.5
  phiarrow1 = phi1*(1-exp(-gamma1*Delta))
  phiarrow2 = phi2*(1-exp(-gamma2*Delta))
  rlTau = r05
  B1Tau = Vasicek.B(gamma=gamma1,T=Tau)
  B2Tau = Vasicek.B(gamma=gamma2,T=Tau)
  ATau = Two.factor.Vasicek.A(phi1,phi2,gamma1,gamma2,sigma1,sigma2,rho,Tau)
  phi10 = (B2Tau*r0-Tau*rlTau-ATau)/(B2Tau-B1Tau)
  phi20 = (Tau*rlTau+ATau-r0*B1Tau)/(B2Tau-B1Tau)

  Z1 = matrix(rnorm(N*M),nrow=N,ncol=M)
  Z2 = rhodelta*Z1+(1-rhodelta^2)^0.5*matrix(rnorm(N*M),nrow=N,ncol=M)

  X1 = matrix(0,nrow=N+1,ncol=M)
  X2 = matrix(0,nrow=N+1,ncol=M)
  X1[1,]= phi10
  X2[1,]= phi20
  for (i in 2:(N+1))
  {
    X1[i,] = X1[i-1,]*exp(-gamma1*Delta) + phiarrow1 + Z1[i-1,]*sigmast1
    X2[i,] = X2[i-1,]*exp(-gamma2*Delta) + phiarrow2+ Z2[i-1,]*sigmast2
  }
  X = X1+X2
  X = ts(X,start=t0,deltat=Delta)
  return(invisible(X))
}
