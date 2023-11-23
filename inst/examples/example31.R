#example31
#similar to example 28
# Two factor Hull-White fit with simulated data example
rm(list=ls()) # clear the workspace and functions
Testdata =subset(VeronesiTable19p4,select=-c(`Cap Price (x100)`))
Testdata$`Cap Prices` = 
  Two.factor.Hull.White.cap.price.vector(cap_rates=Testdata$`Swap Rate`,
                                         Maturity=Testdata$Maturity,
                                         discount_factors=Testdata$discount_factors,
                                         gamma1=0.1,gamma2=-0.2,sigma1=0.02,sigma2=0.03,
                                         rho=-0.4)

Testdata$`Cap Prices`[1]=0

fvol.dat = HW.forward.vol(
  Maturity=Testdata$Maturity,
  discount_factors =Testdata$discount_factors,
  cap_rates=Testdata$`Swap Rate`,
  cap_prices =Testdata$`Cap Prices`)+
  rnorm(length(Testdata$Maturity),0,0.001)
sim.SZ = Two.factor.Vasicek.SZ(TB=fvol.dat$Maturity,
                               TO = fvol.dat$Maturity-0.25,
                               gamma1=0.1,gamma2=-0.2,
                               sigma1=0.02,sigma2=0.03,
                               rho=-0.4)
fvol.dat$sim.SZ = sim.SZ
Two.factor.Hull.White.fit =nls(`Forward Vol.`~ 
                                 Two.factor.Vasicek.SZ(TB=Maturity,
                                                       TO = Maturity-0.25,
                                                       gamma1=gamma1,
                                                       gamma2=gamma2,
                                                       sigma1=sigma1,
                                                       sigma2=sigma2,
                                                       rho=rho),
                               data= fvol.dat,
                               start=list(gamma1=.3,gamma2=-0.3,sigma1=0.1,
                                          sigma2=0.4,rho=0.6),
                               algorithm = "port",
                               upper =list(gamma1=1,gamma2=1,sigma1=1,
                                           sigma2=1,rho=0.9), 
                               lower=list(gamma1=-1,gamma2=-1,sigma1=0.001,
                                          sigma2=0.001,rho=-0.9),
                               nls.control(maxiter = 1000, 
                                           tol = 1e-05, 
                                           minFactor = 1/10240,
                                           printEval = FALSE, 
                                           warnOnly = FALSE, 
                                           scaleOffset = 0,
                                           nDcentral = FALSE))

sum_mod = summary(Two.factor.Hull.White.fit)