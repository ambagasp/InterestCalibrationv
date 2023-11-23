#ciryieldfit
rm(list=ls())
CIR.yield = function(r0=0.02,t=0,T=1,gamma=0.3807,rbar=0.072,alpha=0.0548){
  psi1 = (gamma^2+2*alpha)^0.5
  Denom = (gamma+psi1)*(exp(psi1*(T-t))-1)+ 2*psi1
  B = 2*(exp(psi1*(T-t))-1)/Denom
  A = 2*rbar*gamma/alpha* log((2*psi1*exp((psi1+gamma)*(T-t)/2))/Denom)
  return((-A+B*r0)/(T-t))
}
bond.maturities = seq(0.5,10,0.5) #Time_to_maturity
bond.yields = CIR.yield(r0=0.02,t=0,T=bond.maturities,
                        gamma=0.5,rbar=0.07,alpha=0.05)
bonddata = data.frame(cbind(bond.maturities,bond.yields))
CIR.fit =
  nls(bond.yields~CIR.yield(r0=0.02,t=0,T=bond.maturities,
                            gamma,rbar,alpha),
      start=list(gamma=0.2,rbar=0.02,alpha=0.02),
      data=bonddata,algorithm = "port",
      lower=list(gamma=0.1,rbar=0.01,alpha=0.02),
      upper =list(gamma=3,rbar=1,alpha=1),
      nls.control(maxiter = 1000, tol = 1e-3, minFactor = 1/1024,
                  printEval = FALSE, warnOnly = FALSE,
                  scaleOffset = 0, nDcentral = FALSE))
sum_mod=summary(CIR.fit)