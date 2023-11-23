#example 21
#' Two factor Vasicek risk-neutral calibration with simulated data example
rm(list=ls()) # clear the workspace and functions
bond.maturities =seq(0.5,20,0.5)
set.seed(12345)
bond.yields = Two.factor.Vasicek.yield(t=0,T=bond.maturities,Tau=5,
                                       rt=0.0168,rlTau=0.0452,
                                       phi1=-0.0413,phi2=0,
                                       gamma1=0.8269,gamma2=-0.0288,
                                       short.term.vol = 0.0221, 
                                       long.term.vol = 0.0125,
                                       correlation = 0.4713)
bonddata = data.frame(cbind(bond.maturities,bond.yields))

Two.factor.Vasicek.fit = 
  nls(bond.yields~Two.factor.Vasicek.yield(t=0,T=bond.maturities,
                                           Tau=5,rt=0.0168,
                                           rlTau=0.0452,
                                           phi1,phi2=0,
                                           gamma1,gamma2,
                                           short.term.vol = 0.0221,
                                           long.term.vol = 0.0125, 
                                           correlation = 0.4713),
      start=list(phi1=-0.1,gamma1=0.08,gamma2=-0.1),
      data=bonddata,algorithm = "port",
      lower=list(phi1=-1,gamma1=0,gamma2=-0.5),
      upper =list(phi1=1,gamma1=0.999,gamma2=-0.01),
      nls.control(maxiter = 100, tol = 1e-3, minFactor = 1/1024,
                  printEval = FALSE, warnOnly = FALSE, 
                  scaleOffset = 0, nDcentral = FALSE))
sum_mod = summary(Two.factor.Vasicek.fit)
est.nls=
  Two.factor.Vasicek.Vols(gamma1=as.numeric(coef(Two.factor.Vasicek.fit))[2],
                          gamma2=as.numeric(coef(Two.factor.Vasicek.fit))[3],
                          Tau=5,short.term.vol=0.0221,
                          long.term.vol=0.0125,correlation=0.4713)
