#example 27
rm(list=ls()) # clear the workspace and functions
Data_set =VeronesiTable19p4
Data_set$`Cap Price (x100)` = VeronesiTable19p4$`Cap Price (x100)`/100
Data_set$`Cap Price (x100)`[1]=0
Hull.White.fit =
  nls(Data_set$`Cap Price (x100)`~ Hull.White.cap.price.vector(
    Maturity=Data_set$Maturity,
    cap_rates=Data_set$`Swap Rate`,
    discount_factors=
      Data_set$discount_factors,
    gamma=gamma,
    sigma=sigma),
    data=Data_set ,start=list(gamma=.1,sigma=0.1),
    nls.control(maxiter = 100, tol = 1e-05, 
                minFactor = 1/10240,
                printEval = FALSE, warnOnly = FALSE, 
                scaleOffset = 0,nDcentral = FALSE))
sum_mod = summary(Hull.White.fit)