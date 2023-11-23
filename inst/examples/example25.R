#example 25
# This example uses function ``YieldCurve" package
# first it must be installed and loaded, before running this example.
#
rm(list=ls()) # clear the workspace and functions
TM = VeronesiTable15p1$`Time to Maturity`
Yield =-log(VeronesiTable15p1$Strips/100)/TM*100
xvals = seq(0,10,0.01)
# The function ``Nelson.Siegel" is from the Yield curve package.
NSP = Nelson.Siegel(Yield,TM) 
# The following three functions are developed with this paper.
rates = NSRates(as.numeric(NSP),xvals)
f0t = NSForwards(as.numeric(NSP),xvals)
thetat = Theta(as.numeric(NSP),sigma=0.0221,gamma=0.19, xvals)

plot(xvals,f0t,type="l",lty=2,col="blue",main="Nelson-Siegel fit",
     xlab="Maturity",ylab="Rates(%)" )
points(TM,Yield,type="b",lty=1,col="red")
points(xvals,rates,type="l",lty=1,col="green")
legend("bottomright",legend=c("Forward curve","Current yield",
                              "Fitted Yield"),col=c("blue","red","green"),lty=c(2,1,1))
plot(xvals,thetat,type="l",
     
     main=expression(paste("The function ",
                           theta," (with Nelson-Siegel)")),
     xlab="Time",ylab="Theta")