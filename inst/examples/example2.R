#example2
rm(list=ls()) # clear the workspace and functions
set.seed(123)
X=Vasicek.Euler.sim(Delta=1/252,M=10)
matplot(X,type="l",main="Simulated Vasicek paths (Euler Method)",
        ylab="Short rate",xlab="Time Step in days")