#example20
rm(list=ls()) # clear the workspace and functions
CIR.yield = function(r0=0.02,t=0,T=1,gamma=0.3807,rbar=0.072,alpha=0.0548){
  psi1 = (gamma^2+2*alpha)^0.5
  Denom = (gamma+psi1)*(exp(psi1*(T-t))-1)+ 2*psi1
  B = 2*(exp(psi1*(T-t))-1)/Denom
  A = 2*rbar*gamma/alpha* log((2*psi1*exp((psi1+gamma)*(T-t)/2))/Denom)
  return((-A+B*r0)/(T-t))
}
bond.maturities = VeronesiTable15p1$`Time to Maturity`
bond.prices = VeronesiTable15p1$Strips/100
bond.yields = -log(bond.prices)/bond.maturities
bonddata = data.frame(cbind(bond.maturities,bond.yields))
model = nlm(f=CIR.J,p=c(0.4,0.05,0.03),r0=0.0168,
            bond.prices=bond.prices,iterlim=2000,
            bond.maturities=bond.maturities)
model1 = optim(c(0.4,0.05,0.03),CIR.J,method=("BFGS")
               ,r0=0.0168,bond.prices=bond.prices,
               bond.maturities=bond.maturities)
CIR.fit =
  nls(bond.yields~CIR.yield(r0=0.0168,t=0,T=bond.maturities,
                            gamma,rbar,alpha),
      start=list(gamma=0.4,rbar=0.06,alpha=0.03),
      data=bonddata,algorithm = "port",
      lower=list(gamma=0.1,rbar=0.01,alpha=0.03),
      upper =list(gamma=10,rbar=10,alpha=1),
      nls.control(maxiter = 1000, tol = 1e-3, minFactor = 1/1024,
                  printEval = FALSE, warnOnly = FALSE,
                  scaleOffset = 0, nDcentral = FALSE))
sum_mod= summary(CIR.fit)