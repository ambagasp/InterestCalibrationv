#cir_fit_diagnostics
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
Vasicek.fit =
  nls(bond.yields~Vasicek.yield(r0=0.0168,t=0,T=bond.maturities,
                                gamma,rbar,sigma=0.0221),
      start=list(gamma=0.4,rbar=0.06),
      data=bonddata,algorithm = "port",
      lower=list(gamma=0.1,rbar=0.01),
      upper =list(gamma=10,rbar=10),
      nls.control(maxiter = 1000, tol = 1e-3, minFactor = 1/1024,
                  printEval = FALSE, warnOnly = FALSE,
                  scaleOffset = 0, nDcentral = FALSE))

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

plot(bond.maturities,bond.yields*100,xlab="Maturities",ylab="Yield(%)",
     type="b",pch=10,lty="solid",col="red1")
lines(bond.maturities,fitted(Vasicek.fit)*100,type="b",pch=20,lty="solid",
      col="green3")
lines(bond.maturities,fitted(CIR.fit)*100,type="b",pch=12,col="blue")
legend("bottomright",legend=c("Market Yield(%)","Vasicek fitted yield(%)",
                              "CIR fitted yield(%)"),col=c("red1","green3","blue"),
       lty=c(1,1,1),pch=c(10,20,12))