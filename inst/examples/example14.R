#example14
# The following code snippets calculate the bias of simulated parameters
#CIR Calibration
rm(list=ls()) # clear the workspace and functions
graphics.off() # clear the plots
# CIR Euler estimate
# Initialize parameters
library(matrixStats)
t0=0
T=10
r0=0.02
gamma=0.3807
rbar=0.072
alpha=0.0548
paras = c(gamma,rbar,alpha)
# Initialize # of paths number of points in each path
Delta = 1/252
N = ceiling((T-t0)/Delta)

set.seed(123)
X=CIR.Trans.sim(t0=0,T=10,Delta=1/252,r0=0.02,
                gamma=0.3807,rbar=0.072,alpha=0.0548,M=1000)

M = ncol(X)
euler.est= matrix(NA,3,M)
for (i in 1:M){
  x1 = X[1:N,i]^(-0.5)
  x2 = X[1:N,i]^(0.5)
  y = X[2:(N+1),i]*x1
  model=lm(y~0+x1+x2)
  euler.est[1,i]= (1-as.numeric(model$coefficients)[2])/Delta
  euler.est[2,i] = as.numeric(model$coefficients)[1]/
    (1-as.numeric(model$coefficients)[2])
  euler.est[3,i] = sigma(model)^2/Delta
}

bias=rowMeans(euler.est-paras)
sd =rowSds(euler.est)
rmse =(rowMeans((euler.est-paras)^2))^0.5