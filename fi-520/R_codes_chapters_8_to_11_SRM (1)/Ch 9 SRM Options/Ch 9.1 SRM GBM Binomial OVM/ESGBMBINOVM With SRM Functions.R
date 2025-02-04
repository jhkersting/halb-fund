# ESGBMBINOVM With SRM Functions.R
#  European-style, geometric Brownian motion, binomial OVM  
#  Based on ESGBMBINOVM Functions.R
source('ESGBMBINOVM Functions.R')
# Intrinsic value
OptionIntrinsicValue = function(B){
  with(B, {
    IV = Type*(StockPrice - StrikePrice)
    IV = max(0, IV)
    return( IV )
  })
}
#
# Delta Direct -- have to do full backward recursion
#
ESBINOptionDeltaDirect <- function(B){
  with(B, {
    OHigh <- OLow <- SHigh <- SLow <- 0
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    DriftRate <- (InterestRate - DividendYield)/100.0
    Rate <- InterestRate/100.0  # Local variable, in decimal
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)             # Periodic rate (1+r)
    PeriodDriftRate <- exp(DriftRate*Delta)   # Periodic rate (1+r)
    Prob <- EMMProbability/100.0
    A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    Up <- ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
    Down <- PeriodDriftRate / (Prob*A + (1-Prob))
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
    dSteps <- as.numeric(NumberOfSteps)
    TNS <- NumberOfSteps + 1
    OptionValue <- c(1:TNS)
    for (TimeStep in NumberOfSteps:0){
# Inner loop
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){  # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else { # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - TimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ESBINOptionLowerBound(B)
          OptionValue[i+1] = (1.0/PeriodRate) * (Prob*OptionValue[i+2] +
            (1.0 - Prob)*OptionValue[i+1])
          OptionValue[i+1] = max(LowerBound, OptionValue[i+1])
        }
        if(TimeStep == 1){ # Increment time to maturity and time to PV Div
          if(i == 0){
            OLow <- OptionValue[i+1]
            SLow <- S
          }
          if(i == 1){
            OHigh <- OptionValue[i+1]
            SHigh <- S
          }
        }
      }
    }
    Value <- (OHigh - OLow) / (SHigh - SLow)
    return(Value)
  })
}
#
# Delta Direct Enhanced Method -- have to do full backward recursion
#
ESBINOptionDeltaDirectEnh <- function(B){
  with(B, {
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    TimeToMaturity <- TimeToMaturity + 2.0*Delta
    NumberOfSteps <- NumberOfSteps + 2
    OHigh <- OLow <- SHigh <- SLow <- 0
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    DriftRate <- (InterestRate - DividendYield)/100.0
    Rate <- InterestRate/100.0  # Local variable, in decimal
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)             # Periodic rate (1+r)
    PeriodDriftRate <- exp(DriftRate*Delta)   # Periodic rate (1+r)
    Prob <- EMMProbability/100.0
    A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    Up <- ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
    Down <- PeriodDriftRate / (Prob*A + (1-Prob))
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
    dSteps <- as.numeric(NumberOfSteps)
    TNS <- NumberOfSteps + 1
    OptionValue <- c(1:TNS)
    for (TimeStep in NumberOfSteps:0){
# Inner loop
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){  # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else { # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - TimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ESBINOptionLowerBound(B)
          OptionValue[i+1] = (1.0/PeriodRate) * (Prob*OptionValue[i+2] +
            (1.0 - Prob)*OptionValue[i+1])
          OptionValue[i+1] = max(LowerBound, OptionValue[i+1])
        }
        if(TimeStep == 2){
          if(i == 0){
            OLow <- OptionValue[i+1]
            SLow <- S
          }
          if(i == 2){
            OHigh <- OptionValue[i+1]
            SHigh <- S
          }
        }
      }
    }
    Value <- (OHigh - OLow) / (SHigh - SLow)
    return(Value)
  })
}
#
# Gamma Direct
#
ESBINOptionGammaDirect <- function(B){
  with(B, {
    OHigh <- OMid <- OLow <- SHigh <- SMid <- SLow <- 0
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    DriftRate <- (InterestRate - DividendYield)/100.0
    Rate <- InterestRate/100.0  # Local variable, in decimal
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)             # Periodic rate (1+r)
    PeriodDriftRate <- exp(DriftRate*Delta)   # Periodic rate (1+r)
    Prob <- EMMProbability/100.0
    A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    Up <- ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
    Down <- PeriodDriftRate / (Prob*A + (1-Prob))
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
    dSteps <- as.numeric(NumberOfSteps)
    TNS <- NumberOfSteps + 1
    OptionValue <- c(1:TNS)
    for (TimeStep in NumberOfSteps:0){
# Inner loop
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){  # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else { # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - TimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ESBINOptionLowerBound(B)
          OptionValue[i+1] = (1.0/PeriodRate) * (Prob*OptionValue[i+2] +
            (1.0 - Prob)*OptionValue[i+1])
          OptionValue[i+1] = max(LowerBound, OptionValue[i+1])
        }
        if(TimeStep == 2){
          if(i == 0){
            OLow <- OptionValue[i+1]
            SLow <- S
          }
          if(i == 1){
            OMid <- OptionValue[i+1]
            SMid <- S
          }
          if(i == 2){
            OHigh <- OptionValue[i+1]
            SHigh <- S
          }
        }
      }
    }
    Value <- ( (OHigh - OMid)/(SHigh - SMid) - 
      (OMid - OLow)/(SMid - SLow) ) / (0.5*(SHigh - SLow))
    return(Value)
  })
}
#
# Gamma Direct Enhanced
#
ESBINOptionGammaDirectEnh <- function(B){
  with(B, {
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    TimeToMaturity <- TimeToMaturity + 2.0*Delta
    NumberOfSteps <- NumberOfSteps + 2
    OHigh <- OMid <- OLow <- SHigh <- SMid <- SLow <- 0
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    DriftRate <- (InterestRate - DividendYield)/100.0
    Rate <- InterestRate/100.0  # Local variable, in decimal
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)             # Periodic rate (1+r)
    PeriodDriftRate <- exp(DriftRate*Delta)   # Periodic rate (1+r)
    Prob <- EMMProbability/100.0
    A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    Up <- ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
    Down <- PeriodDriftRate / (Prob*A + (1-Prob))
# Test that d<PeriodicRate<u otherwise quit
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
    dSteps <- as.numeric(NumberOfSteps)
    TNS <- NumberOfSteps + 1
    OptionValue <- c(1:TNS)
    for (TimeStep in NumberOfSteps:0){
# Inner loop
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){  # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else { # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - TimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ESBINOptionLowerBound(B)
          OptionValue[i+1] = (1.0/PeriodRate) * (Prob*OptionValue[i+2] +
            (1.0 - Prob)*OptionValue[i+1])
          OptionValue[i+1] = max(LowerBound, OptionValue[i+1])
        }
        if(TimeStep == 2){
          if(i == 0){
            OLow <- OptionValue[i+1]
            SLow <- S
          }
          if(i == 1){
            OMid <- OptionValue[i+1]
            SMid <- S
          }
          if(i == 2){
            OHigh <- OptionValue[i+1]
            SHigh <- S
          }
        }
      }
    }
    Value <- ( (OHigh - OMid)/(SHigh - SMid) - 
      (OMid - OLow)/(SMid - SLow) ) / (0.5*(SHigh - SLow))
    return(Value)
  })
}



#
# Theta Direct
#
ESBINOptionThetaDirect <- function(B){
  with(B, {
    OMid <- 0
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    DriftRate <- (InterestRate - DividendYield)/100.0
    Rate <- InterestRate/100.0  # Local variable, in decimal
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)             # Periodic rate (1+r)
    PeriodDriftRate <- exp(DriftRate*Delta)   # Periodic rate (1+r)
    # Prob <- EMMProbability/100.0
    # A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    # Up <- ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
    # Down <- PeriodDriftRate / (Prob*A + (1-Prob))
#
# Thetas corrupted unless S(0) = S(ud): Cox, Ross, Rubinstein
#    
    Up <- exp(Sigma*sqrt(Delta))
    Down <- 1/Up
    Prob <- (exp(DriftRate*Delta) - Down) / (Up - Down)
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
    dSteps <- as.numeric(NumberOfSteps)
    TNS <- NumberOfSteps + 1
    OptionValue <- c(1:TNS)
    for (TimeStep in NumberOfSteps:0){
# Inner loop
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){  # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else { # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - TimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ESBINOptionLowerBound(B)
          OptionValue[i+1] = (1.0/PeriodRate) * (Prob*OptionValue[i+2] +
            (1.0 - Prob)*OptionValue[i+1])
          OptionValue[i+1] = max(LowerBound, OptionValue[i+1])
        }
        if(TimeStep == 2){ 
          if(i == 1){
            OMid <- OptionValue[i+1]
            SMid <- S
          }
        }
      }
    }
    Value <- (OMid - OptionValue[1])/(2.0*Delta)
    return(Value)
  })
}
#
# Theta Direct Enhanced
#
ESBINOptionThetaDirectEnh <- function(B){
  with(B, {
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    B$TimeToMaturity <- TimeToMaturity + 2.0*Delta
    B$NumberOfSteps <- NumberOfSteps + 2
    OMid <- 0
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    DriftRate <- (InterestRate - DividendYield)/100.0
    Rate <- InterestRate/100.0  # Local variable, in decimal
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)             # Periodic rate (1+r)
    PeriodDriftRate <- exp(DriftRate*Delta)   # Periodic rate (1+r)
    # Prob <- EMMProbability/100.0
    # A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    # Up <- ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
    # Down <- PeriodDriftRate / (Prob*A + (1-Prob))
#
# Thetas corrupted unless S(0) = S(ud): Cox, Ross, Rubinstein
#    
    Up <- exp(Sigma*sqrt(Delta))
    Down <- 1/Up
    Prob <- (exp(DriftRate*Delta) - Down) / (Up - Down)
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
    dSteps <- as.numeric(NumberOfSteps)
    TNS <- NumberOfSteps + 1
    OptionValue <- c(1:TNS)
    for (TimeStep in NumberOfSteps:0){
# Inner loop
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){  # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else { # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - TimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ESBINOptionLowerBound(B)
          OptionValue[i+1] = (1.0/PeriodRate) * (Prob*OptionValue[i+2] +
            (1.0 - Prob)*OptionValue[i+1])
          OptionValue[i+1] = max(LowerBound, OptionValue[i+1])
        }
        if(TimeStep == 4){
          if(i == 2){
            OMid <- OptionValue[i+1]
            SMid <- S
          }
        }
      }
    }
    Value <- (OMid - OptionValue[1])/(4.0*Delta)
    return(Value)
  })
}
#
# Numerical Greeks
# Delta
ESBINOptionDelta <- function(B){
  Original <- B$StockPrice
  Change <- (B$GreekIncrement/100.0)*Original
  High <- Original + Change
  B$StockPrice <- High
  OHigh <- ESBINOptionValue(B)
  Low <- Original - Change
  B$StockPrice <- Low
  OLow <- ESBINOptionValue(B)
  B$StockPrice <- Original
  OptionDelta <- (OHigh - OLow)/(High - Low)
  return( OptionDelta )
}
# Gamma: Goes to zero for large time steps (UNSTABLE)
ESBINOptionGamma <- function(B){
  Original <- B$StockPrice
  Change <- (B$GreekIncrement/100.0)*Original*100
  High <- Original + Change
  B$StockPrice <- High
  OHigh <- ESBINOptionValue(B)
  Low <- Original - Change
  B$StockPrice <- Low
  OLow <- ESBINOptionValue(B)
  B$StockPrice <- Original
  OMid <- ESBINOptionValue(B)
  OptionGamma <- ( (OHigh - OMid) - (OMid - OLow) )/(Change*Change)
  return(OptionGamma)
}
# Theta
ESBINOptionTheta <- function(B){
  Original <- B$TimeToMaturity
  Change <- (B$GreekIncrement/100.0)*Original*(1/10)
  High <- Original + Change
  B$TimeToMaturity <- High
  OHigh <- ESBINOptionValue(B)
  Low <- Original - Change
  B$TimeToMaturity <- Low
  OLow <- ESBINOptionValue(B)
  B$TimeToMaturity <- Original
  OptionTheta <- (OHigh - OLow)/(High - Low)
  return( -OptionTheta ) 
}
# Vega
ESBINOptionVega <- function(B){
  Original <- B$Volatility
  Change <- (B$GreekIncrement/100.0)*Original
  High <- Original + Change
  B$Volatility <- High
  OHigh <- ESBINOptionValue(B)
  Low <- Original - Change
  B$Volatility <- Low
  OLow <- ESBINOptionValue(B)
  B$Volatility <- Original
  OptionVega <- (OHigh - OLow)/(High - Low)
  return( OptionVega )
}
# Rho
ESBINOptionRho <- function(B){
  Original <- B$InterestRate
  Change <- (B$GreekIncrement/100.0)*Original
  High <- Original + Change
  B$InterestRate <- High
  OHigh <- ESBINOptionValue(B)
  Low <- Original - Change
  B$InterestRate <- Low
  OLow <- ESBINOptionValue(B)
  B$InterestRate <- Original
  OptionRho <- (OHigh - OLow)/(High - Low)
  return( OptionRho )
}
