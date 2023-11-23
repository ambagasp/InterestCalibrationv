#' Calculate forward volatility in Hull-White models.
#' 
#' It uses the function HW.caplet.implied.vol.
#' 
#' @param cap_rates cap rates starting at 0.25 going up to the largest cap
#' @param cap_prices cap prices for each Maturity
#' @param Maturity vector containing 0.25,0.5,0.75 etc
#' @param discount_factors for times 0.25, 0.5,0.75...
#'@export
HW.forward.vol = function(Maturity,discount_factors, cap_rates,cap_prices){
  Nsize = length(Maturity)
  sigma.forward = rep(0,Nsize)
  sigma.forward[2]=uniroot(HW.caplet.implied.vol,c(0,1),
                           price=cap_prices[2],
                           rK= cap_rates[2],
                           T=Maturity[2],
                           ZTO=discount_factors[1],
                           ZTB=discount_factors[2],tol=1e-9)$root
  
  for ( i in (3:Nsize)){
    price.caplet = cap_prices[i]-
      Hull.White.capv2(rK= cap_rates[i],
                       TK= Maturity[i-1],
                       discount_factors = discount_factors[1:i],
                       sigmaf = sigma.forward[2:(i-1)])
    sigma.forward[i] = 
      uniroot(HW.caplet.implied.vol,c(0,1),
              price=price.caplet, 
              rK= cap_rates[i],
              T=Maturity[i],
              ZTO=discount_factors[i-1],
              ZTB=discount_factors[i],tol=1e-9)$root
  }
  fvol.dat = data.frame(Maturity[2:Nsize],sigma.forward[2:Nsize])
  fvol.dat = setNames(fvol.dat,c("Maturity","Forward Vol."))
  return(fvol.dat)
}