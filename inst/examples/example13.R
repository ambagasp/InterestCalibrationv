#example13
# clear the workspace and functions
rm(list=ls())

bond.prices = VeronesiTable15p1$Strips/100
bond.maturities = VeronesiTable15p1$`Time to Maturity`
bond.yields = -log(bond.prices)/bond.maturities
bonddata = data.frame(cbind(bond.maturities,bond.yields))
model = nlm(f=Vasicek.J,p=c(0.4,0.06),r0=0.0168,sigma=0.0221,
            bond.prices=bond.prices,bond.maturities=bond.maturities)
model1 = optim(c(0.4,0.06),Vasicek.J,method=("CG"),sigma=0.0221
               ,bond.prices=bond.prices,bond.maturities=bond.maturities)

Vasicek.fit =
  nls(bond.yields~Vasicek.yield(r0=0.0168,t=0,T=bond.maturities,
                                gamma,rbar,sigma=0.0221),
      start=list(gamma=0.4,rbar=0.06),
      data=bonddata,algorithm = "port",
      lower=list(gamma=0.1,rbar=0.01),
      upper =list(gamma=10,rbar=10),
      nls.control(maxiter = 1000, tol = 1e-3, minFactor = 1/1024,
                  printEval = FALSE, warnOnly = FALSE,
                  scaleOffset = 0, nDcentral = FALSE))
sum_mod= summary(Vasicek.fit)