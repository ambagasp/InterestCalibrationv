#example12
# clear the workspace and functions
rm(list=ls())

bond.maturities = seq(0.5,10,0.5) #Time_to_maturity
bond.prices = Vasicek.zcbp(r0=0.02,t=0,T=bond.maturities,
                           gamma=0.5,rbar=0.07,sigma=0.02)
model = nlm(f=Vasicek.J,p=c(0.4,0.02,0.04),r0=0.02,iterlim=1000,
            bond.prices=bond.prices,bond.maturities=bond.maturities)
model1 = optim(c(0.4,0.02,0.04),lower=c(0,0,0),Vasicek.J,
               method=("L-BFGS-B"),r0=0.02,
               bond.prices=bond.prices,
               bond.maturities=bond.maturities)