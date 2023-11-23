#example18
rm(list=ls()) # clear the workspace and functions
bond.maturities = seq(0.5,10,0.5)
bond.prices = CIR.zcbp(r0=0.02,t=0,T=bond.maturities,gamma=0.5,
                       rbar=0.07,alpha=0.05)
model = nlm(f=CIR.J,p=c(0.3,0.05,0.02),r0=0.02,
            bond.prices=bond.prices,
            bond.maturities=bond.maturities)
model1 = optim(c(0.3,0.05,0.02),lower = c(0,0,0),CIR.J,method=("L-BFGS-B")
               ,r0=0.02,bond.prices=bond.prices,
               bond.maturities=bond.maturities)