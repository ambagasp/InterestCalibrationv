# first nlsexample1
rm(list=ls())
bond.maturities = seq(0.5,10,0.5) #Time_to_maturity
bond.yields = Vasicek.yield(r0=0.02,t=0,T=bond.maturities,
                            gamma=0.5,rbar=0.07,sigma=0.02)
bonddata = data.frame(cbind(bond.maturities,bond.yields))
Vasicek.fit =
  nls(bond.yields~Vasicek.yield(r0=0.02,t=0,T=bond.maturities,
                                gamma,rbar,sigma),
      start=list(gamma=0.2,rbar=0.02,sigma=0.01),
      data=bonddata,algorithm = "port",
      lower=list(gamma=0.1,rbar=0.01,sigma=0.005),
      upper =list(gamma=3,rbar=1,sigma=1),
      nls.control(maxiter = 100, tol = 1e-3, minFactor = 1/1024,
                  printEval = FALSE, warnOnly = FALSE,
                  scaleOffset = 0, nDcentral = FALSE))
sum_mod= summary(Vasicek.fit)