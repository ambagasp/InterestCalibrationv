#example30
rm(list=ls()) # clear the workspace and functions
fvol.dat = HW.forward.vol(Maturity=VeronesiTable19p4$Maturity,
                          discount_factors =VeronesiTable19p4$discount_factors,
                          cap_rates=VeronesiTable19p4$`Swap Rate`,
                          cap_prices =VeronesiTable19p4$`Cap Price (x100)`/100 )
Hull.White.fit =nls(`Forward Vol.`~ Vasicek.SZ(TB=Maturity,
                                               TO = Maturity-0.25,
                                               gamma=gamma,
                                               sigma=sigma),
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