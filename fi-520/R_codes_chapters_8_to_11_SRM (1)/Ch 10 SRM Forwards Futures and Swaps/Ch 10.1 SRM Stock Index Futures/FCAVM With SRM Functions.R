# FCAVM With Greeks Functions.R
#
#
# Core fuctions
#
# Future value function for FCA-related calculations
#
FVwDAI = function(Maturity, Rate, DividendYield){
  return(exp( ((Rate - DividendYield)/100.0) * Maturity) )
}

FVwDA = function(FV){
  with(FV, {
    return(exp( ((InterestRate - DividendYield)/100.0) * TimeToMaturity) )
  })
}
# Futures Value based on Carry Arbitrage
FVCA = function(FV){
  with(FV, {
    return( IndexPrice*FVwDA(FV) )
  })
}
# Futures Value based on Carry Arbitrage Delta
FVCADelta = function(FV){
  with(FV, {
    return( FVwDA(FV) )
  })
}
# Futures Value based on Carry Arbitrage Gamma
FVCAGamma = function(FV){
  with(FV, {
    return( 0 )
  })
}
# Futures Value based on Carry Arbitrage Theta
FVCATheta = function(FV){
  with(FV, {
    return( -((InterestRate - DividendYield)/100.0) * FVCA(FV) )
  })
}
# Futures Value based on Carry Arbitrage Vega
FVCAVega = function(FV){
  with(FV, {
    return( 0 )
  })
}
# Futures Value based on Carry Arbitrage Rho
FVCARho = function(FV){
  with(FV, {
    return( TimeToMaturity * FVCA(FV) )
  })
}


