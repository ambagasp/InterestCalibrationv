#optimexample
optim.yield =Vasicek.yield(r0=0.0168,t=0,T=bond.maturities,
                           gamma=model$estimate[1],rbar=model$estimate[2],
                           sigma=0.0222) 
nlm.yield =Vasicek.yield(r0=0.0168,t=0,T=bond.maturities,
                         gamma=model1$par[1],rbar=model1$par[2],
                         sigma=0.0222) 
text.yield = Vasicek.yield(r0=0.0168,t=0,T=bond.maturities,
                           gamma=.4653,rbar=0.0634,sigma=0.0222) 
plot(bond.maturities,bond.yields*100,xlab="Maturities",ylab="Yield(%)",
     type="b",pch=10,lty="solid",col="red1")
lines(bond.maturities,fitted(Vasicek.fit)*100,type="b",pch=20,lty="solid",
      col="rosybrown")
lines(bond.maturities,optim.yield*100,type="b",pch=12,col="green4")
lines(bond.maturities,nlm.yield*100,type="b",pch=8,col="royalblue")
lines(bond.maturities,text.yield*100,type="b",pch=13,col="magenta")
legend("bottomright",legend=c("Market Yield(%)","nls Yield(%)",
                              "optim Yield(%)","nlm Yield(%)","Text Yield(%)"),
       col=c("red1","rosybrown","green4","royalblue","magenta"),
       lty=c(1,1,1),pch=c(10,20,12,8,13))