#example9
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
  model2 = arima(X[1:(N+1),i],order=c(1,0,0),include.mean = TRUE,
                 method="ML")
  mle.gamma1= -log(as.numeric(model2$coef)[1])/Delta
  mle.rbar1 = as.numeric(model2$coef)[2]
  mle.sigma1= (model2$sigma2*2*mle.gamma1/
                 (1-as.numeric(model2$coef)[1]^2))^.5
  mle[,i]=c(mle.gamma1,mle.rbar1,mle.sigma1)
}
# Simulation performance criteria
bias =rowMeans(mle-paras)
sd = rowSds(mle)
rmse = rowMeans((mle-paras)^2)^0.5