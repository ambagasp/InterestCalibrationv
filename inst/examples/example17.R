#example17
rm(list=ls()) # clear the workspace and functions
bond.maturities = seq(0.5,10,0.5)
bond.prices = CIR.zcbp(r0=0.02,t=0,T=bond.maturities,gamma=0.5,
                       rbar=0.07,alpha=0.05)
model = nlm(f=CIR.J,p=c(0.4,0.05),r0=0.02,alpha=0.05,
            bond.prices=bond.prices,
            bond.maturities=bond.maturities)
model1 = optim(c(0.4,0.05),CIR.J,method=("BFGS")
               ,r0=0.02,alpha=0.05,
               bond.prices=bond.prices,
               bond.maturities=bond.maturities)
