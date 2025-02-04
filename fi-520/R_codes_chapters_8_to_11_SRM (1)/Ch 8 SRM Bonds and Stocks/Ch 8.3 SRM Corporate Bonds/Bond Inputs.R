# Bond Inputs.R
#  Contains both individual UST and CMT curve information
#  Single UST bond
# Test inputs: BB Corporate Bond
ActualBondPrice = 99.289    # Percent of par
inputFrequency = 2L         # Coupon frequency per year, 1, 2, 4, or 12
inputCouponRate = 3.8       # Percent
inputPar = 100.0            # Currency
inputYieldToMaturity = 5.30  # Percent
# Dollars: Quoted bond price without accrued interest (stubbed to -99)
inputBondPrice = -99       
SettlementDateMonth = 6      # Integer: 1-12
SettlementDateDay = 21       # Integer: 1-31
SettlementDateYear = 2020    # Integer: 1-very high number
MaturityDateMonth = 6        # Integer: 1-12
MaturityDateDay = 21         # Integer: 1-31
MaturityDateYear = 2030      # Integer: 1-very high number
inputChangeInYTM = 0.001     # Percent
#
# CMT curve information
#
# NFactors <- 4 # Number of factors including Level, 8 or less
# NBaseCurve <- 30 # Potential observation for every year for 30 years
#
# Input current CMT yields
#
MarketCMTRates <- numeric(NBaseCurve) # num vector size NBaseCurve, zeros
MarketCMTRates[MarketCMTRates == 0] <- NA # Replace with NAs

MarketCMTRates[1] <- CMT$CMTYield[1]
MarketCMTRates[2] <- CMT$CMTYield[2]
MarketCMTRates[3] <- CMT$CMTYield[3]
MarketCMTRates[5] <- CMT$CMTYield[4]
MarketCMTRates[7] <- CMT$CMTYield[5]
MarketCMTRates[10] <- CMT$CMTYield[6]
MarketCMTRates[20] <- CMT$CMTYield[7]
MarketCMTRates[30] <- CMT$CMTYield[8]
#
# Input current Spread  yields (BB yields, not spreads)
#
MarketAllInRates <- numeric(NBaseCurve) # num vector size NBaseCurve, zeros
MarketAllInRates[MarketAllInRates == 0] <- NA # Replace with NAs
MarketAllInRates[1] <- 3.248
MarketAllInRates[2] <- 3.535
MarketAllInRates[3] <- 3.814
MarketAllInRates[4] <- 4.115
MarketAllInRates[5] <- 4.386
MarketAllInRates[6] <- 4.64
MarketAllInRates[7] <- 4.863	
MarketAllInRates[8] <- 5.058
MarketAllInRates[9] <- 5.208
MarketAllInRates[10] <- 5.351 
MarketAllInRates[15] <- 5.989
MarketAllInRates[20] <- 6.521
MarketAllInRates[25] <- 6.543
MarketAllInRates[30] <- 6.55
# Input Scalars: Taus for CMT and Spreads (???)
NSCTau <- NSCFactors - 2   # Must be between 0 and 7 (not inclusive, integer)
SCTau <- numeric(NSCTau)   # b (Level, slope, and curvatures)
SCTau[1] <- 2.0 # Overwritten with bond maturity in test program
if(NSCTau>1)SCTau[2] <- 3.0
if(NSCTau>2)SCTau[3] <- 6.0
if(NSCTau>3)SCTau[4] <- 15.0
if(NSCTau>4)SCTau[5] <- 0.5
if(NSCTau>5)SCTau[6] <- 4.0
