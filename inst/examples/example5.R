#example5
rm(list=ls()) # clear the workspace and functions
set.seed(123)
X =CIR.Trans.sim(Delta=1/252,M=10)
matplot(X,type="l",main="Simulated CIR paths (Transition Density Method)",
        ylab="Short rate",xlab="Time Step in days")
abline(0,0)