#example15
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
                gamma=0.3807,rbar=0.072,alpha=0.0548,M=100)

M = ncol(X)
euler.est= matrix(NA,3,M)
mle.est = matrix(NA,3,M)
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
  model1 = optim(euler.est[,i],CIR.log.lik,NULL,X[,i],method="BFGS")
  mle.est[,i]= model1$par
  if (!is.null(model1$message))
    Wsample = append(Wsample,i)
}

bias.euler= rowMeans(euler.est-paras)
sd.euler= rowSds(euler.est)
rmse.euler =(rowMeans((euler.est-paras)^2))^0.5
bias.mle =rowMeans(mle.est-paras)
sd.mle= rowSds(mle.est)
rmse.mle = (rowMeans((mle.est-paras)^2))^0.5