#example23
rm(list=ls()) # clear the workspace and functions
bond.maturities = VeronesiTable15p1$`Time to Maturity`
bond.yields = -log(VeronesiTable15p1$Strips/100)/bond.maturities
bonddata = data.frame(cbind(bond.maturities,bond.yields))
Vasicek.fit =
  nls(bond.yields~Vasicek.yield(r0=0.0168,t=0,T=bond.maturities,
                                gamma,rbar,sigma=0.0221),
      start=list(gamma=0.4,rbar=0.06),
      data=bonddata,algorithm = "port",
      lower=list(gamma=0.1,rbar=0.01),
      upper =list(gamma=10,rbar=10),
      nls.control(maxiter = 100, tol = 1e-3, minFactor = 1/1024,
                  printEval = FALSE, warnOnly = FALSE,
                  scaleOffset = 0, nDcentral = FALSE))


Two.factor.Vasicek.fit = nls(bond.yields~Two.factor.Vasicek.yield(t=0,
                                                                  T=bond.maturities,
                                                                  Tau=5,rt=0.0168,rlTau=0.0452,
                                                                  phi1,phi2=0,gamma1,gamma2,
                                                                  short.term.vol = 0.0221,
                                                                  long.term.vol = 0.0125, 
                                                                  correlation = 0.4713),
                             start=list(phi1=-0.08,gamma1=0.7,gamma2=-0.04),
                             data=bonddata,algorithm = "port",
                             lower=list(phi1=-0.9,gamma1=-2,gamma2=-2),
                             upper =list(phi1=0.9,gamma1=2,gamma2=2),
                             nls.control(maxiter = 10000, tol = 1e-8, 
                                         minFactor = 1/10240,
                                         printEval = FALSE, 
                                         warnOnly = TRUE, scaleOffset = 0,
                                         nDcentral = FALSE),trace=FALSE)


plot(bond.maturities,bond.yields*100,xlab="Maturities",ylab="Yield(%)",
     type="b",pch=10,lty="solid",col="red1")
lines(bond.maturities,fitted(Vasicek.fit)*100,type="b",pch=20,
      lty="solid",col="green3")
lines(bond.maturities,fitted(Two.factor.Vasicek.fit)*100,type="b",
      pch=12,col="blue")
legend("bottomright",legend=c("Market Yield(%)","Vasicek fitted yield(%)",
                              "Two factor Vasicek fitted yield(%)"),
       col=c("red1","green3","blue"),lty=c(1,1,1),pch=c(10,20,12))
