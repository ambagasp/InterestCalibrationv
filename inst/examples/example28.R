#example 28
# Hull-White fit with simulated data example
rm(list=ls()) # clear the workspace and functions
Testdata2 = VeronesiTable19p4
Testdata2$`Cap Price (x100)` = 
  Two.factor.Hull.White.cap.price.vector(
    cap_rates=VeronesiTable19p4$`Swap Rate`,
    Maturity=VeronesiTable19p4$Maturity,
    discount_factors=VeronesiTable19p4$discount_factors,
    gamma1=0.1,gamma2=-0.2,sigma1=0.2,sigma2=0.3,rho=0.5)

Testdata2$`Cap Price (x100)`[1]=0


Two.factor.Hull.White.fit =nls(`Cap Price (x100)`~ 
                                 Two.factor.Hull.White.cap.price.vector(
                                   cap_rates=Testdata2$`Swap Rate`,
                                   Maturity=Testdata2$Maturity,
                                   discount_factors=Testdata2$discount_factors,
                                   gamma1=gamma1,gamma2=gamma2,sigma1=sigma1,sigma2=sigma2,rho=rho),
                               data= Testdata2,
                               start=list(gamma1=.3,gamma2=-0.3,sigma1=0.1,sigma2=0.4,rho=0.6),
                               nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/1024,
                                           printEval = FALSE, warnOnly = FALSE, scaleOffset = 0,
                                           nDcentral = FALSE))
summary(Two.factor.Hull.White.fit)