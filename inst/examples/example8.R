#example8
# clear the workspace and functions
rm(list=ls())
t0=0
T=10
r0=0.03
gamma=0.3
rbar=0.05
sigma=0.0221
paras = c(gamma,rbar,sigma)
# Initialize # of paths number of points in each path
Delta = 1/252
N = ceiling((T-t0)/Delta)

set.seed(123)
X=Vasicek.Trans.sim(t0,T,Delta,r0,gamma,rbar,sigma,M=1000)

M = ncol(X)
mle= matrix(0,3,M)
for (i in 1:M){
  y = X[2:(N+1),i]
  x = X[1:N,i]
  model=lm(y~x)
  mle.gamma= -log(as.numeric(model$coefficients)[2])/Delta
  mle.rbar = as.numeric(model$coefficients)[1]/
    (1-as.numeric(model$coefficients)[2])
  mle.sigma = sigma(model)*(2*mle.gamma/
                              (1-as.numeric(model$coefficients)[2]^2))^.5
  mle[,i]=c(mle.gamma,mle.rbar,mle.sigma)
}
# Simulation performance criteria
bias =rowMeans(mle-paras)
Sd = rowSds(mle)
rmse = rowMeans((mle-paras)^2)^0.5