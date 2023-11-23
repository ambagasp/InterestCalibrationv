#example6
rm(list=ls()) # clear the workspace and functions
set.seed(123)
X =Two.factor.Vasicek.Trans.sim(Delta=1/252,M=10)
matplot(X,type="l",main="Simulated two factor Vasicek paths",
        ylab="Short rate",xlab="Time Step in days")