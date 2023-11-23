#example4
rm(list=ls()) # clear the workspace and functions
set.seed(123)
X =CIR.Euler.sim(Delta=1/500,M=10)
matplot(X,type="l",main="Simulated CIR paths (Euler Method)",
        ylab="Short rate",xlab="Time Step in days")
abline(0,0)