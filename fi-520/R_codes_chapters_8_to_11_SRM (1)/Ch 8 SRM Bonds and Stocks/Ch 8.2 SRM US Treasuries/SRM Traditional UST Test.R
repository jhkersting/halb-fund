# SRM Traditional UST Test.R
# rmarkdown::render("Valuation UST Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
#  optimx - general purpose optimization
Packages <- c("date", "optimx") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#
# Test Inputs for U. S. Treasury bonds
#
MarketQuotedBondPrice = 91 + 18/32 # Dollars: Quoted price without accrued interest
inputFrequency = 2L         # Coupon frequency per year, 1, 2, 4, or 12
inputCouponRate = 2.25      # Percent
inputPar = 100.0        # Currency
inputYieldToMaturity = 4.2558853  # Percent
# Dollars: Quoted bond price without accrued interest (stubbed to -99)
inputBondPrice = -99       
SettlementDateMonth = 3     # Integer: 1-12
SettlementDateDay = 10      # Integer: 1-31
SettlementDateYear = 2023   # Integer: 1-very high number
MaturityDateMonth = 11      # Integer: 1-12
MaturityDateDay = 15        # Integer: 1-31
MaturityDateYear = 2027     # Integer: 1-very high number
inputChangeInYTM = 0.001
#
# UST functions (semi-annual only)
#
source("SRM UST Functions.R")
BONDInputData <- list(inputFrequency, inputCouponRate, inputPar, 
  inputYieldToMaturity, inputBondPrice, 
  SettlementDateMonth, SettlementDateDay, SettlementDateYear, 
  MaturityDateMonth, MaturityDateDay, MaturityDateYear,
  inputChangeInYTM)
names(BONDInputData) <- c("Frequency", "CouponRate", "Par", 
  "YieldToMaturity", "BondPrice", 
  "SettlementDateMonth", "SettlementDateDay", "SettlementDateYear", 
  "MaturityDateMonth", "MaturityDateDay", "MaturityDateYear",
  "ChangeInYTM")
#
# Calendar manipulations
#
N = CouponsRemaining(BONDInputData)
# ElapsedOutput contains fraction, JLastDate, JNextDate, and JCurrentDate
ElapsedOutput = Elapsed(BONDInputData)
# Number of Total Days
NTD <- ElapsedOutput$NextDate - ElapsedOutput$LastDate
# Number of Accrued Days since last semi-annual coupon
NAD <- ElapsedOutput$Fraction * NTD
# Fraction of coupon period that has elapsed already
f <- ElapsedOutput$Fraction
# Bond maturity, in years
Mat <- TimeToMaturity(BONDInputData)
NAD; NTD; f; N; Mat
#
# Bond value given yield to maturity
#
MarketValueOfBond = BondValue(BONDInputData)
AccruedInterestAmount = AccruedInterest(BONDInputData)
ModelQuotedBondPrice = MarketValueOfBond - AccruedInterestAmount
MarketValueOfBond; AccruedInterestAmount; 
ModelQuotedBondPrice; MarketQuotedBondPrice
#
# Yield to maturity given bond value
#
inputBondPrice = MarketQuotedBondPrice #Dollars:Quoted price w/o accrued interest
BONDInputData$BondPrice <- inputBondPrice
EstYieldToMaturity = YieldToMaturitySolver(BONDInputData)
EstYieldToMaturity; inputYieldToMaturity
#
# Static risk management values
#
TestMacDuration = MacDuration(BONDInputData)  # Macaulay duration
TestDuration = Duration(BONDInputData)  # Modified duration
TestConvexity = Convexity(BONDInputData) # Standard convexity
TestEffectiveDuration = EffectiveDuration(BONDInputData) 
TestEffectiveConvexity = EffectiveConvexity(BONDInputData) 
TestMacDuration; TestDuration; TestConvexity; TestEffectiveDuration; TestEffectiveConvexity



# #
# # Selected Graphs
# #
# CouponIncrement = inputCouponRate/2.0  # Not used in function, just graphs
# NumberOfObservations = 101L
# LowerBound = 0
# UpperBound = 20
# HCoupon = inputCouponRate + CouponIncrement
# LCoupon = inputCouponRate - CouponIncrement
# StepSize = (UpperBound - LowerBound)/(NumberOfObservations - 1)
# # Plots 
# Y <- as.double(c(1:NumberOfObservations))
# OBV <- as.double(c(1:NumberOfObservations))
# ODuration <- as.double(c(1:NumberOfObservations))
# OConvexity <- as.double(c(1:NumberOfObservations))
# HBV <- as.double(c(1:NumberOfObservations))
# HDuration <- as.double(c(1:NumberOfObservations))
# HConvexity <- as.double(c(1:NumberOfObservations))
# LBV <- as.double(c(1:NumberOfObservations))
# LDuration <- as.double(c(1:NumberOfObservations))
# LConvexity <- as.double(c(1:NumberOfObservations))
# originalYTM = BONDInputData$YieldToMaturity
# originalCR = BONDInputData$CouponRate
# for(i in 1:NumberOfObservations){
#   Y[i] <- LowerBound + (i-1)*StepSize
#   BONDInputData$YieldToMaturity = Y[i]
#   BONDInputData$CouponRate = originalCR
#   OBV[i] = BondValue(BONDInputData) #- AccruedInterest(BONDInputData)
#   ODuration[i] = Duration(BONDInputData)
#   OConvexity[i] = Convexity(BONDInputData)
#   # High Coupon  
#   BONDInputData$CouponRate = HCoupon
#   HBV[i] = BondValue(BONDInputData) #- AccruedInterest(BONDInputData)
#   HDuration[i] = Duration(BONDInputData)
#   HConvexity[i] = Convexity(BONDInputData)
#   # Low Coupon
#   BONDInputData$CouponRate = LCoupon
#   LBV[i] = BondValue(BONDInputData) #- AccruedInterest(BONDInputData)
#   LDuration[i] = Duration(BONDInputData)
#   LConvexity[i] = Convexity(BONDInputData)
#   BONDInputData$CouponRate = originalCR
# }
# BONDInputData$YieldToMaturity = originalYTM
# BONDInputData$CouponRate = originalCR
# # BV
# MaxValue = max(Y); MinValue = min(Y)
# xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
# MaxValue = max(LBV, OBV, HBV); MinValue = min(LBV, OBV, HBV)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# mTitle = "Bond Values"
# xTitle = "Yield To Maturity"
# yTitle = "Value"
# Trade = paste0('SD=', SettlementDateMonth,'/',SettlementDateDay,'/',
#   SettlementDateYear)
# Maturity = paste0(',MD=', MaturityDateMonth,'/',MaturityDateDay,'/',
#   MaturityDateYear)
# Par = paste0(',Par=', inputPar)
# Coupon = paste0(',C =', LCoupon, ',', inputCouponRate, ',', HCoupon)
# Freq = paste0(',F=',inputFrequency)
# legtxt = c("Low Coupon","Coupon","High Coupon")
# lTitle = "Parameter"
# sTitle = paste0(Trade,Maturity,Par,Coupon,Freq)
# plot(Y, LBV, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(Y, OBV, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
#   pch = 2, cex = 0.5)
# lines(Y, HBV, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
#   pch = 3, cex = 0.5)
# legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1), 
#   col = c("black","black","black"), pch = c(1, 2, 3), bty = "n", 
#   title = lTitle)
# #
# # Duration graph
# #
# MaxValue = max(LDuration, ODuration, HDuration)
# MinValue = min(LDuration, ODuration, HDuration)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# mTitle = "Macaulay Duration"
# xTitle = "Yield To Maturity"
# yTitle = "Macaulay Duration"
# Trade = paste0('SD=', SettlementDateMonth, '/', SettlementDateDay,'/',
#   SettlementDateYear)
# Maturity = paste0(',MD=', MaturityDateMonth, '/', MaturityDateDay,'/',
#   MaturityDateYear)
# Par = paste0(',Par=', inputPar)
# Coupon = paste0(',C=', LCoupon, ',', inputCouponRate, ',', HCoupon)
# Freq = paste0(',F=',inputFrequency)
# legtxt = c("Low Coupon","Coupon","High Coupon")
# lTitle = "Parameter"
# sTitle = paste(Trade,Maturity,Par,Coupon,Freq)
# plot(Y, LDuration, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(Y, ODuration, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
#   pch = 2, cex = 0.5)
# lines(Y, HDuration, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
#   pch = 3, cex = 0.5)
# legend("bottomleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1), 
#   col = c("black","black","black"), pch = c(1, 2, 3), bty = "n", 
#   title = lTitle)
# #
# # Convexity graph
# #
# MaxValue = max(LConvexity, OConvexity, HConvexity)
# MinValue = min(LConvexity, OConvexity, HConvexity)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# mTitle = "Standard Convexity"
# xTitle = "Yield To Maturity"
# yTitle = "Standard Convexity"
# Trade = paste0('SD=', SettlementDateMonth, '/', SettlementDateDay, '/',
#   SettlementDateYear)
# Maturity = paste0(',MD=', MaturityDateMonth, '/', MaturityDateDay, '/',
#   MaturityDateYear)
# Par = paste0(',Par=', inputPar)
# Coupon = paste0(',C=', LCoupon, ',', inputCouponRate, ',', HCoupon)
# Freq = paste0(',F=',inputFrequency)
# legtxt = c("Low Coupon","Coupon","High Coupon")
# lTitle = "Parameter"
# sTitle = paste(Trade,Maturity,Par,Coupon,Freq)
# plot(Y, LConvexity, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(Y, OConvexity, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
#   pch = 2, cex = 0.5)
# lines(Y, HConvexity, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
#   pch = 3, cex = 0.5)
# legend("bottomleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1), 
#   col = c("black","black","black"), pch = c(1, 2, 3), bty = "n", 
#   title = lTitle)
