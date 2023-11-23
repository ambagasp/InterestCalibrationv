#example32
rm(list=ls()) # clear the workspace and functions
# first calculate cap price vector similar to Veronesi Table 19.4
Testdata =subset(VeronesiTable19p4,select=-c(`Cap Price (x100)`))
Testdata$`Cap Prices` =
  Two.factor.Hull.White.cap.price.vector(cap_rates=Testdata$`Swap Rate`,
                                         Maturity=Testdata$Maturity,
                                         discount_factors=Testdata$discount_factors,
                                         gamma1=0.1,gamma2=-0.2,sigma1=0.2,sigma2=0.3,rho=-0.2)

Testdata$`Cap Prices`[1]=0
# Now calculate forward volatility from the cap prices.
fvol.dat = HW.forward.vol(Maturity=Testdata$Maturity,
                          discount_factors =Testdata$discount_factors,
                          cap_rates=Testdata$`Swap Rate`,
                          cap_prices =Testdata$`Cap Prices`)

fvol.dat$`Forward Vol.sq` = fvol.dat$`Forward Vol.`^2/(fvol.dat$Maturity-0.25)

st = c(gamma1=0.4,gamma2=0.4,sigma1=0.4,sigma2=0.5,rho=-0.1)
paras = st
Two.factor.Hull.White.fit=nlfb(start=st, resfn=Two.factor.Vasicek.SZ.res,
                               jacfn=Two.factor.Vasicek.SZ.gradient,data=fvol.dat,
                               trace=FALSE, control=list(prtlvl=1),forward_vol.dat=fvol.dat)
sum_mod = summary(Two.factor.Hull.White.fit)