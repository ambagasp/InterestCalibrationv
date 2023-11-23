#example10
# clear the workspace and functions
rm(list=ls())
t0=0
T=20
r0=0.03
gamma=0.3
rbar=0.05
sigma=0.02
# Initialize # of paths number of points in each path
Delta = 1/252
N = ceiling((T-t0)/Delta)
set.seed(123)
# simulate M sample paths and retain the last row only.
X=Vasicek.Trans.sim(t0,T,Delta,r0,gamma,rbar,sigma,M=100)[N+1,]
qs = quantile(X,c(0.025,0.975))
est.gamma = 2*((1.96*sigma)/(as.numeric(qs[2])-as.numeric(qs[1])))^2
est.rbar = (as.numeric(qs[2])+as.numeric(qs[1]))/2