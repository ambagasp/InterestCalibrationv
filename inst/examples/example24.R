#example 24
rm(list=ls()) # clear the workspace and functions
TM = VeronesiTable15p1$`Time to Maturity`
Yield = VeronesiTable15p1$`Yield %`*100
model1 = splinefun(TM,Yield,method="natural")
sigma=0.0196
gamma=0.19
model2 = lm(Yield~poly(TM,6,raw=TRUE))
coefs = as.numeric(coef(model2))
xvals = seq(0,10,0.25)
rates = sapply(xvals,polyfit,coeffs=coefs)
ratesdash = sapply(xvals,polyfit,coeffs=coefs,deriv=1)
rates2dash = sapply(xvals,polyfit,coeffs=coefs,deriv=2)
f0t = rates+ratesdash*xvals
thetat = 2*ratesdash/100+xvals*rates2dash/100+sigma^2*xvals+
  gamma*f0t/100+sigma^2/(2*gamma)*(1-exp(-2*gamma*xvals))
plot(xvals,f0t,type="l",lty=2,col="blue",
     main="6th degree polynomial fit",xlab="Maturity",ylab="Rates(%)" )
points(TM,Yield,type="b",lty=1,col="red")
points(xvals,rates,type="l",lty=1,col="green")
legend("bottomright",legend=c("Forward curve","Current yield",
                              "Fitted Yield"),
       col=c("blue","red","green"),lty=c(2,1,1))
plot(xvals,thetat,type="l",
     main=expression(paste("The function ",
                           theta," (with 6 degree polynomial)")),
     xlab="Time",ylab="Theta")