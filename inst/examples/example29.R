#example29
#simulate a data set as in example 26
# Hull-White fit with simulated data example
rm(list=ls()) # clear the workspace and functions
Testdata =subset(VeronesiTable19p4,select=-c(`Cap Price (x100)`))
Testdata$`Cap Prices` = 
  Hull.White.cap.price.vector(
    Maturity=Testdata$Maturity,
    cap_rates=Testdata$`Swap Rate`,
    discount_factors=Testdata$discount_factors,
    gamma=.1,sigma=0.015)+rnorm(length(Testdata$Maturity),0,0.00001)
Testdata$`Cap Prices`[1]=0
fvol.dat = HW.forward.vol(Maturity=Testdata$Maturity,
                          discount_factors =Testdata$discount_factors,
                          cap_rates=Testdata$`Swap Rate`,
                          cap_prices =Testdata$`Cap Prices` )
Hull.White.fit =nls(`Forward Vol.`~ Vasicek.SZ(
  TB=Maturity,
  TO = Maturity-0.25,
  gamma=gamma,sigma=sigma),
  data= fvol.dat,
  start=list(gamma=.3,sigma=0.1),
  algorithm = "port",
  upper =list(gamma=5,sigma=1), 
  lower=list(gamma=-5,sigma=0.001),
  nls.control(maxiter = 1000000, 
              tol = 1e-05, 
              minFactor = 1/10240,
              printEval = FALSE, 
              warnOnly = FALSE, 
              scaleOffset = 0,
              nDcentral = FALSE))
sum_mod = summary(Hull.White.fit)