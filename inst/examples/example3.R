#example3
rm(list=ls()) # clear the workspace and functions
set.seed(123)
X =Vasicek.Trans.sim(Delta=1/252,M=10)
matplot(X,type="l",main="Simulated Vasicek paths (Transition Density Method)",
        ylab="Short rate",xlab="Time Step in days")