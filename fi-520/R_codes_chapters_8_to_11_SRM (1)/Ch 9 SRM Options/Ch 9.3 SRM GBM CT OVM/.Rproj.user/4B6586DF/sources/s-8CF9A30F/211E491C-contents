# ASGBMBINOVM With SRM Functions.R
#  American-style, geometric Brownian motion, binomial OVM  
# Present value function (See ESGBMBINOVM With SRM Functions.R)
# Binomial probability
source('ESGBMBINOVM With SRM Functions.R')
# Built on ASGBMBINOVM Functions.R
source('ASGBMBINOVM Functions.R')
#
# Delta Direct -- have to do full backward recursion
#
ASBINOptionDeltaDirect <- function(B){
  with(B, {
    # OriginalTimeToMaturity <- TimeToMaturity
    # OriginalStockPrice <- StockPrice
    OHigh <- OLow <- SHigh <- SLow <- 0
    N <- NumberOfSteps + 1
    OptionValue <- c(1:N)
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    Rate <- InterestRate/100.0  
    DriftRate <- (InterestRate - DividendYield)/100.0  
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)
    PeriodDriftRate <- exp(DriftRate*Delta)
    Prob <- EMMProbability/100.0
    A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    Up <- ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
    Down <- PeriodDriftRate / (Prob*A + (1-Prob))
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
    dSteps <- as.numeric(NumberOfSteps)
    for (TimeStep in NumberOfSteps:0){
      dTimeStep <- as.numeric(TimeStep)
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){ # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else {                           # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - dTimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ASBINOptionLowerBound(B)
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
# Delta Direct Enhanced (+2 Delta) -- have to do full backward recursion
#
ASBINOptionDeltaDirectEnh <- function(B){
  with(B, {
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    TimeToMaturity <- TimeToMaturity + 2.0*Delta
    NumberOfSteps <- NumberOfSteps + 2
    OHigh <- OLow <- SHigh <- SLow <- 0
    N <- NumberOfSteps + 1
    OptionValue <- c(1:N)
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    Rate <- InterestRate/100.0  
    DriftRate <- (InterestRate - DividendYield)/100.0  
    Sigma <- Volatility/100.0   # Local variable, in decimal
    PeriodRate <- exp(Rate*Delta)
    PeriodDriftRate <- exp(DriftRate*Delta)
    AdjustedStockPrice <- StockPrice 
    Prob <- EMMProbability/100.0
    A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    Up <- ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
    Down <- PeriodDriftRate / (Prob*A + (1-Prob))
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
    dSteps <- as.numeric(NumberOfSteps)
    for (TimeStep in NumberOfSteps:0){
      dTimeStep <- as.numeric(TimeStep)
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){ # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else { # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - dTimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ASBINOptionLowerBound(B)
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
ASBINOptionGammaDirect <- function(B){
  with(B, {
    OHigh <- OLow <- SHigh <- SLow <- SMid <- OMid <- 0
    N <- NumberOfSteps + 1
    OptionValue <- c(1:N)
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    Rate <- InterestRate/100.0  
    DriftRate <- (InterestRate - DividendYield)/100.0  
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)
    PeriodDriftRate <- exp(DriftRate*Delta)
    AdjustedStockPrice <- StockPrice 
    Prob <- EMMProbability/100.0
    A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    Up <- ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
    Down <- PeriodDriftRate / (Prob*A + (1-Prob))
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
    dSteps <- as.numeric(NumberOfSteps)
    for (TimeStep in NumberOfSteps:0){
      dTimeStep <- as.numeric(TimeStep)
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){ # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else { # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - dTimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ASBINOptionLowerBound(B)
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
ASBINOptionGammaDirectEnh <- function(B){
  with(B, {
    OriginalTimeToMaturity <- TimeToMaturity
    OriginalStockPrice <- StockPrice
    OriginalNumberOfSteps <- NumberOfSteps
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    TimeToMaturity <- TimeToMaturity + 2.0*Delta
    NumberOfSteps <- NumberOfSteps + 2
    OHigh <- OLow <- SHigh <- SLow <- SMid <- OMid <- 0
    N <- NumberOfSteps + 1
    OptionValue <- c(1:N)
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    Rate <- InterestRate/100.0  
    DriftRate <- (InterestRate - DividendYield)/100.0  
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)
    PeriodDriftRate <- exp(DriftRate*Delta)
    AdjustedStockPrice <- StockPrice 
    Prob <- EMMProbability/100.0
    A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
    Up <- ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
    Down <- PeriodDriftRate / (Prob*A + (1-Prob))
    if(Up < PeriodRate || Down > PeriodRate)return(-99)
    dSteps <- as.numeric(NumberOfSteps)
    for (TimeStep in NumberOfSteps:0){
      dTimeStep <- as.numeric(TimeStep)
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){ # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else { # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - dTimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ASBINOptionLowerBound(B)
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
ASBINOptionThetaDirect <- function(B){
  with(B, {
    OriginalTimeToMaturity <- TimeToMaturity
    OriginalStockPrice <- StockPrice
    OHigh <- OLow <- SHigh <- SLow <- SMid <- OMid <- 0
    N <- NumberOfSteps + 1
    OptionValue <- c(1:N)
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    Rate <- InterestRate/100.0  
    DriftRate <- (InterestRate - DividendYield)/100.0  
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)
    PeriodDriftRate <- exp(DriftRate*Delta)
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
    for (TimeStep in NumberOfSteps:0){
      dTimeStep <- as.numeric(TimeStep)
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){  # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else { # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - TimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ASBINOptionLowerBound(B)
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
ASBINOptionThetaDirectEnh <- function(B){
  with(B, {
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    TimeToMaturity <- TimeToMaturity + 2.0*Delta
    NumberOfSteps <- NumberOfSteps + 2
    OHigh <- OLow <- SHigh <- SLow <- SMid <- OMid <- 0
    N <- NumberOfSteps + 1
    OptionValue <- c(1:N)
    Sum <- 0
    Moneyness <- 0
    Value <- 0
    Rate <- InterestRate/100.0  
    DriftRate <- (InterestRate - DividendYield)/100.0  
    Sigma <- Volatility/100.0   # Local variable, in decimal
    Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
    PeriodRate <- exp(Rate*Delta)
    PeriodDriftRate <- exp(DriftRate*Delta)
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
    for (TimeStep in NumberOfSteps:0){
      for (i in 0:TimeStep){
        S <- (Up^i) * (Down^(TimeStep-i)) * StockPrice
        if(TimeStep == NumberOfSteps){  # At expiration
          OptionValue[i+1] = max(0, Type*(S - StrikePrice))
        } else { # Prior to expiration
          B$TimeToMaturity <- TimeToMaturity - TimeStep*Delta
          B$StockPrice <- S
          LowerBound <- ASBINOptionLowerBound(B)
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
#
# Delta
ASBINOptionDelta <- function(B){
  Original <- B$StockPrice
  Change <- (B$GreekIncrement/100.0)*Original
  High <- Original + Change
  B$StockPrice <- High
  OHigh <- ASBINOptionValue(B)
  Low <- Original - Change
  B$StockPrice <- Low
  OLow <- ASBINOptionValue(B)
  B$StockPrice <- Original
  OptionDelta <- (OHigh - OLow)/(High - Low)
  return( OptionDelta )
}
# Gamma: Goes to zero for large time steps (UNSTABLE)
ASBINOptionGamma <- function(B){
  Original <- B$StockPrice
  Change <- (B$GreekIncrement/100.0)*Original*100
  High <- Original + Change
  B$StockPrice <- High
  OHigh <- ASBINOptionValue(B)
  Low <- Original - Change
  B$StockPrice <- Low
  OLow <- ASBINOptionValue(B)
  B$StockPrice <- Original
  OMid <- ASBINOptionValue(B)
  OptionGamma <- ( (OHigh - OMid) - (OMid - OLow) )/(Change*Change)
  # return(OMid - OLow)
  return(OptionGamma)
}
# Theta
ASBINOptionTheta <- function(B){
  Original <- B$TimeToMaturity
  Change <- (B$GreekIncrement/100.0)*Original*(1/10)
  High <- Original + Change
  B$TimeToMaturity <- High
  OHigh <- ASBINOptionValue(B)
  Low <- Original - Change
  B$TimeToMaturity <- Low
  OLow <- ASBINOptionValue(B)
  B$TimeToMaturity <- Original
  OptionTheta <- (OHigh - OLow)/(High - Low)
  # Moving time to maturity up is maturity time down (-)
  return( -OptionTheta )
}
# Vega
ASBINOptionVega <- function(B){
  Original <- B$Volatility
  Change <- (B$GreekIncrement/100.0)*Original
  High <- Original + Change
  B$Volatility <- High
  OHigh <- ASBINOptionValue(B)
  Low <- Original - Change
  B$Volatility <- Low
  OLow <- ASBINOptionValue(B)
  B$Volatility <- Original
  OptionVega <- (OHigh - OLow)/(High - Low)
  return( OptionVega )
}
# Rho
ASBINOptionRho <- function(B){
  Original <- B$InterestRate
  Change <- (B$GreekIncrement/100.0)*Original
  High <- Original + Change
  B$InterestRate <- High
  OHigh <- ASBINOptionValue(B)
  Low <- Original - Change
  B$InterestRate <- Low
  OLow <- ASBINOptionValue(B)
  B$InterestRate <- Original
  OptionRho <- (OHigh - OLow)/(High - Low)
  return( OptionRho )
}


#SCRAPS
# # American-style lower bound
# ASBINOptionLowerBound <- function(B){
#   with(B, {
#     LowerBound <- Type * (StockPrice * PV1(TimeToMaturity, DividendYield) -
#       StrikePrice * PV1(TimeToMaturity, InterestRate))
#     IntrinsicValue <- Type * (StockPrice - StrikePrice)
#     LowerBound <- max(0, LowerBound, IntrinsicValue)
#     return( LowerBound )
#   })
# }
# # American-style upper bound
# ASBINOptionUpperBound <- function(B){
#   with(B, {
#     if(Type == 1){
#       UpperBound <- 
#         StockPrice * PV1(TimeToMaturity, DividendYield)
#       UpperBound <- min(UpperBound, StockPrice)
#     }
#     if(Type == -1){
#       UpperBound <- 
#         StrikePrice * PV1(TimeToMaturity, InterestRate)
#       UpperBound <- max(UpperBound, max(0, StrikePrice - StockPrice))
#     }
#     return( UpperBound )
#   })
# }
# #
# # American-style binomial option valuation function
# #
# ASBINOptionValue <- function(B){
#   with(B, {
#     N <- NumberOfSteps + 1
#     OptionValue <- c(1:N)
#     Sum <- 0
#     Moneyness <- 0
#     Value <- 0
#     Rate <- InterestRate/100.0  
#     DriftRate <- (InterestRate - DividendYield)/100.0  
#     Sigma <- Volatility/100.0   # Local variable, in decimal
#     Delta <- TimeToMaturity / as.numeric(NumberOfSteps)
#     PeriodRate <- exp(Rate*Delta)
#     PeriodDriftRate <- exp(DriftRate*Delta)
#     
#     
#     
#     AdjustedStockPrice <- StockPrice * 
#       exp(-(DividendYield/100.0)*TimeToMaturity) # Incorporate dividends (?)
#     
#     
#     
#     Prob <- EMMProbability/100.0
#     A <- exp( Sigma*sqrt(Delta)/sqrt(Prob*(1-Prob)) )
#     Up <- ( PeriodDriftRate * A ) / (Prob*A + (1-Prob))
#     Down <- PeriodDriftRate / (Prob*A + (1-Prob))
# # Test that d<PeriodicRate<u otherwise quit
#     if(Up < PeriodRate || Down > PeriodRate)return(-99)
#     dSteps <- as.numeric(NumberOfSteps)
#     for (TimeStep in NumberOfSteps:0){
#       dTimeStep <- as.numeric(TimeStep)
#       for (i in 0:TimeStep){
#         if(TimeStep == NumberOfSteps){ # At expiration
#           if( (Type == 1) && (PayoutType == 1) ){    # Plain vanilla call
#             Moneyness = (Up^i) * (Down^(TimeStep-i)) * AdjustedStockPrice -
#               StrikePrice
#             OptionValue[i+1] = max(0, Moneyness)
#           }
#           if( (Type == 1) && (PayoutType == 2) ){    # Digital call
#             Moneyness = (Up^i)*(Down^(TimeStep-i))*AdjustedStockPrice -
#               StrikePrice
#             if(Moneyness > 0.0) Moneyness = DigitalPayout
#             OptionValue[i+1] = max(0, Moneyness)
#           }
#           if( (Type == -1) && (PayoutType == 1) ){   # Plain vanilla put
#             Moneyness = StrikePrice -
#               ((Up^i)*(Down^(TimeStep-i)) * AdjustedStockPrice)
#             OptionValue[i+1] = max(0, Moneyness)
#           }
#           if( (Type == -1) && (PayoutType == 2) ){  # Digital put
#             Moneyness  = StrikePrice -
#               ((Up^i)*(Down^(TimeStep-i)) * AdjustedStockPrice)
#             if(Moneyness > 0.0) Moneyness = DigitalPayout
#             OptionValue[i+1] = max(0, Moneyness)
#           }
#         } else { # Prior to expiration
#           AStockPrice = (Up^i) * (Down^(TimeStep-i)) * AdjustedStockPrice  
#             
# # Check lower boundary condition and intrinsic value
# # Find appropriate Time to Matuirty            
#           TTM = TimeToMaturity - dTimeStep*Delta
#           LowerBound <- max( 0, Type * (StockPrice * PV1(TTM, DividendYield) -
#             StrikePrice * PV1(TTM, InterestRate)) )
#           if(PayoutType == 2)LowerBound <- 0
# 
#           # if(PayoutType == 1){    # Plain vanilla option
#           #   LowerBound = Type * (AStockPrice - 
#           #     StrikePrice * PV1(TTM, InterestRate))
#           # } else {
#           #   LowerBound = 0
#           # }
#             
#           OptionValue[i+1] = (1.0/PeriodRate) * (Prob*OptionValue[i+2] +
#             (1.0 - Prob)*OptionValue[i+1])
#           if( (Type == 1) && (PayoutType == 1) ){    # Plain vanilla call
#             Moneyness = AStockPrice - StrikePrice
#             if(Style == 1){
#               OptionValue[i+1] = max(LowerBound, OptionValue[i+1])
#             } else {
#               OptionValue[i+1] = max(LowerBound, Moneyness, OptionValue[i+1])
#             }
#           }
#           if( (Type == 1) && (PayoutType == 2) ){    # Digital call
#             Moneyness = AStockPrice - StrikePrice
#             if(Moneyness > 0.0) Moneyness = DigitalPayout
#             if(Style == 1){
#               OptionValue[i+1] = max(LowerBound, OptionValue[i+1])
#             } else {
#               OptionValue[i+1] = max(LowerBound, Moneyness, OptionValue[i+1])
#             }
#           }
#           if( (Type == -1) && (PayoutType == 1) ){   # Plain vanilla put
#             Moneyness = StrikePrice - AStockPrice
#             if(Style == 1){
#               OptionValue[i+1] = max(LowerBound, OptionValue[i+1])
#             } else {
#               OptionValue[i+1] = max(LowerBound, Moneyness, OptionValue[i+1])
#             }
#           }
#           if( (Type == -1) && (PayoutType == 2) ){  # Digital put
#             Moneyness  = StrikePrice - AStockPrice
#             if(Moneyness > 0.0) Moneyness = DigitalPayout
#             if(Style == 1){
#               OptionValue[i+1] = max(LowerBound, OptionValue[i+1])
#             } else {
#               OptionValue[i+1] = max(LowerBound, Moneyness, OptionValue[i+1])
#             }
#           }
#         }
#       }
#     }
# # Check lower boundary conditions
#     if(PayoutType == 1){    # Plain vanilla option
#       LowerBound <- ASBINOptionLowerBound(B)
#     } else {
#       LowerBound = 0
#     }
#     Value = max(OptionValue[1], LowerBound)
#     return(Value)
#   })
# }        




