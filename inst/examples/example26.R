#example 26
# Hull-White fit with simulated data example
rm(list=ls()) # clear the workspace and functions
Testdata2 = VeronesiTable19p4
Testdata2$`Cap Price (x100)` = 
  Hull.White.cap.price.vector(
    Maturity=VeronesiTable19p4$Maturity,
    cap_rates=VeronesiTable19p4$`Swap Rate`,
    discount_factors=VeronesiTable19p4$discount_factors,
    gamma=.1,sigma=0.015)+rnorm(20,0,0.0001)

Hull.White.fit = 
  nls(`Cap Price (x100)`~ 
        Hull.White.cap.price.vector(Maturity=Testdata2$Maturity,
                                    cap_rates=Testdata2$`Swap Rate`,
                                    discount_factors=Testdata2$discount_factors,
                                    gamma=gamma,sigma=sigma),
      data= Testdata2,start=list(gamma=.1,sigma=0.01),
      nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/1024,
                  printEval = FALSE, warnOnly = FALSE, scaleOffset = 0,
                  nDcentral = FALSE))
sum_mod = summary(Hull.White.fit)