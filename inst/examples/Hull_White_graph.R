
# Hull-White fit graphical illustration 
plot(`Cap Price (x100)`~ `Swap Rate`, data =Data_set,
     xlab = "Swap Rate", ylab = "Cap Price")
lines(Data_set$`Swap Rate`, fitted(Hull.White.fit))
plot(`Cap Price (x100)`~ `Maturity`, data =Data_set,
     xlab = "Maturity", ylab = "Cap Price")
lines(Data_set$`Maturity`, fitted(Hull.White.fit))
plot(fitted(Hull.White.fit),residuals(Hull.White.fit),xlab="Fitted Values",
     ylab="Residuals")
abline(a=0,b=0)