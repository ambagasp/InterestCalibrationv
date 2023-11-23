#example16
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
                gamma=0.3807,rbar=0.072,alpha=0.0548,M=10)

M = ncol(X)
euler.est= matrix(NA,3,M)
gmm.est = matrix(NA,3,M)
Wsample = NULL
for (i in 1:M){
  x1 = X[1:N,i]^(-0.5)
  x2 = X[1:N,i]^(0.5)
  y = X[2:(N+1),i]*x1
  model=lm(y~0+x1+x2)
  euler.est[1,i]= (1-as.numeric(model$coefficients)[2])/Delta
  euler.est[2,i] = as.numeric(model$coefficients)[1]/
    (1-as.numeric(model$coefficients)[2])
  euler.est[3,i] = sigma(model)^2/Delta
  model2 = optim(c(as.numeric(model$coefficients)[1],
                   as.numeric(model$coefficients)[2],sigma(model))
                 ,CIR.gmm,NULL,X[,i],method="L-BFGS-B")
  gmm.est[1,i] = (1- model2$par[2])/Delta
  gmm.est[2,i] = model2$par[1]/(1-model2$par[2])
  gmm.est[3,i] = model2$par[3]^2/Delta
}
# performance measurements
bias.euler= rowMeans(euler.est-paras)
sd.euler= rowSds(euler.est)
rmse.euler =(rowMeans((euler.est-paras)^2))^0.5
bias.gmm =rowMeans(gmm.est-paras)
sd.gmm= rowSds(gmm.est)
rmse.gmm = (rowMeans((gmm.est-paras)^2))^0.5