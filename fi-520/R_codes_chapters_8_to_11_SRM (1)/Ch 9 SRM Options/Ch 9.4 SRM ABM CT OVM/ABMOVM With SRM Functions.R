# ABMOVM w Greeks Functions.R
#
#  INPUT STRUCTURE
#  ABMInputData - list of inputs with associated names; percent, not decimal
#  ABMInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
#    inputDividendYield, inputVolatility, inputTimeToMaturity, inputType)
#  names(ABMInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
#    "DividendYield", "Volatility", "TimeToMaturity", "Type")
# Available core functions
#  PV1(Maturity, Rate) - present value of $1
#  B = ABMInputData
#  d1(B) - value of d1
#  d2(B) - value of d2
#  n(d) - standard normal PDF, given scalar d
#  N(d) - standard normal CDF, given scalar d
# ABM value functions
#  ABMOptionValue(B) - option value, type = 1 is call, type = 2 is put
#  OptionLowerBound(B) - option lower bounds
#  OptionUpperBound(B) - option upper bounds
# ABM greeks functions
#  ABMOptionDelta(B)
#  ABMOptionGamma(B)
#  ABMOptionTheta(B)
#  ABMOptionVega(B)
#  ABMOptionRho(B)
# ABM implied functions
#  ABMDYOptionImpliedVolatility(B, inputOptionValue)
#  ABMDYOptionImpliedStockPrice(B, inputOptionValue)
#  ABMDYOptionImpliedStrikePrice(B, inputOptionValue)
#  ABMDYOptionImpliedTimeToMaturity(B, inputOptionValue)
#  ABMDYOptionImpliedInterestRate(B, inputOptionValue)
#  ABMDYOptionImpliedDividendYield(B, inputOptionValue)
#
# Core fuctions
#
# Functions for ABMOVM-related calculations
#
PV1 = function(Maturity, Rate){
  return(exp( -(Rate/100.0) * Maturity) )
}
# Adjusted volatility to use in dn and option values
AdjustedSigma = function(B){
  with(B, {
    if (abs(InterestRate - DividendYield)<0.00001){
      AdjSigma <- Volatility * sqrt(TimeToMaturity)
    } else {
      AdjSigma <- Volatility * 
        sqrt( (( PV1(TimeToMaturity,-2*(InterestRate-DividendYield)) )-1) /
                (2*(InterestRate-DividendYield)/100) )
    }
    return(AdjSigma)
  })
}
# dn - functions used in ABMOVM
# with: Evaluates R expressions based on a set of data (B here)
dn = function(B){
  with(B, {
    AdjSigma <- AdjustedSigma(B)
    Num = StockPrice*PV1(TimeToMaturity, -(InterestRate-DividendYield)) -
      StrikePrice
    Den = AdjSigma 
    return( Num/Den )
  })
}
# Normal distribution functions
# n - probability density function of standard normal (0,1)
n = function(d){
  return( exp( -(d^2) / 2.0 ) / ( sqrt( 2.0 * pi ) ) )
}
# N - cumulative distribution function of standard normal (0,1)
N = function(d){
  return( as.numeric(integrate(n,-Inf,d)[1]) )
}
# ABMOVM
ABMOptionValue = function(B){
  with(B, {
    AdjSigma <- AdjustedSigma(B)
    OptionValue = Type * ( StockPrice * PV1(TimeToMaturity, DividendYield) - 
                             StrikePrice * PV1(TimeToMaturity, InterestRate) ) * N(Type * dn(B)) +
      PV1(TimeToMaturity, InterestRate) * AdjSigma * n(dn(B))
    LowerBound = Type * StockPrice * PV1(TimeToMaturity, DividendYield) -
      Type * StrikePrice * PV1(TimeToMaturity, InterestRate)
    return( max(OptionValue, LowerBound) )
  })
}
# Lower bound
OptionLowerBound = function(B){
  with(B, {
    LowerBound = Type * StockPrice * PV1(TimeToMaturity, DividendYield) -
      Type * StrikePrice * PV1(TimeToMaturity, InterestRate)
    LowerBound = max(0, LowerBound)
    return( LowerBound )
  })
}
# Upper bound
OptionUpperBound = function(B){
  with(B, {
    if(Type == 1)UpperBound = StockPrice * PV1(TimeToMaturity, DividendYield)
    if(Type == -1)UpperBound = StrikePrice * PV1(TimeToMaturity, InterestRate)
    return( UpperBound )
  })
}
# GREEKS
# Delta
ABMOptionDelta = function(B){
  with(B, {
    OptionDelta = Type * PV1(TimeToMaturity, DividendYield) * N(Type * dn(B)) 
    return( OptionDelta )
  })
}
# Gamma
ABMOptionGamma = function(B){
  with(B, {
    OptionGamma = ( PV1(TimeToMaturity, -(InterestRate - 2.0* DividendYield)) * 
      n(dn(B)) ) /
      AdjustedSigma(B)
    return( OptionGamma )
  })
}
# Theta
ABMOptionTheta = function(B){
  with(B, {
    OptionTheta = Type * ( (DividendYield/100.0) * 
      PV1(TimeToMaturity, DividendYield) * StockPrice -
      (InterestRate/100.0)*PV1(TimeToMaturity, InterestRate)*StrikePrice ) *
      N(Type*dn(B)) +
      AdjustedSigma(B) * (InterestRate/100.0) * 
      PV1(TimeToMaturity, InterestRate) * n(dn(B)) -
      ( (Volatility^2) * PV1(TimeToMaturity,-(InterestRate-2.0*DividendYield)) ) /
      (2.0 * AdjustedSigma(B))*n(dn(B))
    return( OptionTheta )
  })
}
# Vega
ABMOptionVega = function(B){
  with(B, {
    OptionVega = ( PV1(TimeToMaturity, InterestRate) * AdjustedSigma(B) * 
      n(dn(B)) ) / Volatility
    return( OptionVega )  
  })
}
# Rho
ABMOptionRho = function(B){
  with(B, {
    if(abs(InterestRate - DividendYield) < 0.00001){
      OptionRho = TimeToMaturity * PV1(TimeToMaturity, InterestRate) *
        ( Type * StrikePrice * N(Type * dn(B)) - 
        AdjustedSigma(B) * n(dn(B)) ) +                               
        ( ( Volatility^2 * PV1(TimeToMaturity, InterestRate) * n(dn(B)) ) /
        (2.0 * AdjustedSigma(B)) ) * (TimeToMaturity^2)
    } else {
      OptionRho = TimeToMaturity * PV1(TimeToMaturity, InterestRate) *
        ( Type * StrikePrice * N(Type * dn(B)) - 
        AdjustedSigma(B) * n(dn(B)) ) +                               
        ( ( Volatility^2 * PV1(TimeToMaturity, InterestRate) * n(dn(B)) ) /
        (2.0 * AdjustedSigma(B)) ) * 
        ( (PV1(TimeToMaturity, -2.0*(InterestRate - DividendYield)) * 
        (2.0 * ((InterestRate - DividendYield)/100.0) * TimeToMaturity - 
        1.0 ) + 1.0) /
        (2.0 * ((InterestRate - DividendYield)/100.0)^2) )
    }
    return( OptionRho/100.0 )  # Report in decimal form
  })
}


#
# Rho
#
ABMOptionNRho <- function(B){
  Original <- B$InterestRate
  Change <- 0.0001
  SHigh <- Original + Change
  B$InterestRate <- SHigh
  OHigh <- ABMOptionValue(B)
  SLow <- Original - Change
  B$InterestRate <- SLow
  OLow <- ABMOptionValue(B)
  B$InterestRate <- Original
  NRho <- (OHigh - OLow)/(SHigh - SLow)
  return( NRho )
}


