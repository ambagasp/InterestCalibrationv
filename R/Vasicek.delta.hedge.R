#' delta hedging of zero coupon bond
#' @param r0 initial rate
#' @param T1 maturity of short bond
#' @param T2 maturity of long bond
#' @param delta portfolio rebalancing time intervals
#' @param gamma is speed of mean reversion in real world
#'@param gammast is speed of mean reversion in risk neutral world
#'@param rbar is the long term mean in real world.
#' @param rbarst is the long term mean in risk neutral world.
#'@param sigma is the instantaneous volatility.
#'
#'@export
Vasicek.delta.hedge = function(r0=0.0168, T1=1.1041,T2=3.3479,delta=1/252, gamma=0.3261,
                       rbar=0.0509,gammast= 0.4653,rbarst=0.0634,sigma=0.0221){
  Interest_rate_path = Vasicek.Trans.sim(t0=0,T=T1,Delta=delta,r0,M=1)
  N = floor(T1/delta)
  BT1 = Vasicek.ZCBP1(r0,t=0,T=T1,gamma=gammast,rbar=rbarst,sigma=0.0221)
  BT2 = Vasicek.ZCBP1(r0,t=0,T=T2,gamma=gammast,rbar=rbarst,sigma=0.0221)
  Delta0 = BT1[1,3]*BT1[1,4]/(BT2[1,3]*BT2[1,4])
  Cash = BT1[1,4]-Delta0*BT2[1,4]
  Outputs = matrix(0,nrow=N,ncol=5)
  Outputs[1,] =c(Interest_rate_path[1],BT1[1,4],BT2[1,4],Delta0,Cash)
  for (i in (2:N)){
    t = (i-1)*delta
    BT1 = Vasicek.ZCBP1(r0=Interest_rate_path[i],t=t,T=T1,gamma=gammast,rbar=rbarst,sigma=0.0221)
    BT2 = Vasicek.ZCBP1(r0=Interest_rate_path[i],t=t,T=T2,gamma=gammast,rbar=rbarst,sigma=0.0221)
    Delta1 = BT1[1,3]*BT1[1,4]/(BT2[1,3]*BT2[1,4])
    Cash = Cash*exp(Interest_rate_path[i-1]*delta)-(Delta1-Delta0)*BT2[1,4]
    Delta0 = Delta1
    Outputs[i,] =c(Interest_rate_path[i],BT1[1,4],BT2[1,4],Delta1,Cash)
  }
  Outputs = ts(Outputs,start=0,deltat=delta)
  return(Outputs)
}
