#'Price of a caplet  with Hull-White model.
#'
#'It is based on that fact that caplet price can be viewed as put option on a zero-coupon bond.
#'@param rK cap rate.
#'@param  T is the time of caplet payment.
#'@param Delta is the spacing of payments, normally its 0.25
#'@param  ZT1 is the discount factor for time T-Delta.
#'@param  ZT2 is the discount factor for time T.
#'@param gamma Hull-White gamma.
#'@param sigma Hull-White sigma.
#'@export
Hull.White.caplet = function(rK= 0.03815,T=5,Delta=0.25,ZT1=0.835326,ZT2=0.8247441,gamma=0.054523,sigma=0.0149){
  BTOTB = Vasicek.B(gamma,Delta)
  SZ = (BTOTB^2*sigma^2*Vasicek.B((2*gamma),(T-Delta)))^0.5
  K = 1/(1+rK*Delta)
  d1 = 1/SZ*log(ZT2/(K*ZT1))+ SZ/2
  d2 = d1-SZ
  return(ZT1*pnorm(-d2)-ZT2/K*pnorm(-d1))
}
