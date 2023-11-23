#example22
rm(list=ls()) # clear the workspace and functions
bond.maturities = VeronesiTable15p1$`Time to Maturity`
bond.yields = -log(VeronesiTable15p1$Strips/100)/bond.maturities
bonddata = data.frame(cbind(bond.maturities,bond.yields))
Two.factor.Vasicek.fit = 
  nls(bond.yields~Two.factor.Vasicek.yield(t=0,T=bond.maturities,
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
sum_mod = summary(Two.factor.Vasicek.fit)
est.nls= Two.factor.Vasicek.Vols(
  gamma1=as.numeric(coef(Two.factor.Vasicek.fit))[2],
  gamma2=as.numeric(coef(Two.factor.Vasicek.fit))[3],
  Tau=5,short.term.vol=0.0221,long.term.vol=0.0125,correlation=0.4713)