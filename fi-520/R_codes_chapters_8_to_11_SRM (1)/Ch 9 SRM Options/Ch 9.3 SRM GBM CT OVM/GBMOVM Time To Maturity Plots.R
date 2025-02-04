# GBMOVM Time To Maturity Plots.R
#   Sub program for Module 9.3
# Plots with StockPrice
StepSize = (UpperBoundT - LowerBoundT)/(NumberOfObservations - 1)
TimeToMaturity <- c(1:NumberOfObservations)
CallGBMValue <- c(1:NumberOfObservations)
PutGBMValue <- c(1:NumberOfObservations)
CallEuropeanBinomialValue <- c(1:NumberOfObservations)
PutEuropeanBinomialValue <- c(1:NumberOfObservations)
CallAmericanBinomialValue <- c(1:NumberOfObservations)
PutAmericanBinomialValue <- c(1:NumberOfObservations)
CallGBMDelta <- c(1:NumberOfObservations)
PutGBMDelta <- c(1:NumberOfObservations)
CallEuropeanBinomialDelta <- c(1:NumberOfObservations)
PutEuropeanBinomialDelta <- c(1:NumberOfObservations)
CallAmericanBinomialDelta <- c(1:NumberOfObservations)
PutAmericanBinomialDelta <- c(1:NumberOfObservations)
CallGBMGamma <- c(1:NumberOfObservations)
PutGBMGamma <- c(1:NumberOfObservations)
CallEuropeanBinomialGamma <- c(1:NumberOfObservations)
PutEuropeanBinomialGamma <- c(1:NumberOfObservations)
CallAmericanBinomialGamma <- c(1:NumberOfObservations)
PutAmericanBinomialGamma <- c(1:NumberOfObservations)
CallGBMTheta <- c(1:NumberOfObservations)
PutGBMTheta <- c(1:NumberOfObservations)
CallEuropeanBinomialTheta <- c(1:NumberOfObservations)
PutEuropeanBinomialTheta <- c(1:NumberOfObservations)
CallAmericanBinomialTheta <- c(1:NumberOfObservations)
PutAmericanBinomialTheta <- c(1:NumberOfObservations)
CallGBMVega <- c(1:NumberOfObservations)
PutGBMVega <- c(1:NumberOfObservations)
CallEuropeanBinomialVega <- c(1:NumberOfObservations)
PutEuropeanBinomialVega <- c(1:NumberOfObservations)
CallAmericanBinomialVega <- c(1:NumberOfObservations)
PutAmericanBinomialVega <- c(1:NumberOfObservations)
CallGBMRho <- c(1:NumberOfObservations)
PutGBMRho <- c(1:NumberOfObservations)
CallEuropeanBinomialRho <- c(1:NumberOfObservations)
PutEuropeanBinomialRho <- c(1:NumberOfObservations)
CallAmericanBinomialRho <- c(1:NumberOfObservations)
PutAmericanBinomialRho <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  TimeToMaturity[i] <- LowerBoundT + (i - 1)*StepSize
  GBMInputData$TimeToMaturity = TimeToMaturity[i]
  BINInputData$TimeToMaturity = TimeToMaturity[i]
  GBMInputData$Type = 1
  CallGBMValue[i] <- GBMOptionValue(GBMInputData)
  CallGBMDelta[i] <- GBMOptionDelta(GBMInputData)
  CallGBMGamma[i] <- GBMOptionGamma(GBMInputData)
  CallGBMVega[i] <- GBMOptionVega(GBMInputData)
  CallGBMTheta[i] <- GBMOptionTheta(GBMInputData)
  CallGBMRho[i] <- GBMOptionRho(GBMInputData)
  GBMInputData$Type = -1
  PutGBMValue[i] <- GBMOptionValue(GBMInputData)
  PutGBMDelta[i] <- GBMOptionDelta(GBMInputData)
  PutGBMGamma[i] <- GBMOptionGamma(GBMInputData)
  PutGBMVega[i] <- GBMOptionVega(GBMInputData)
  PutGBMTheta[i] <- GBMOptionTheta(GBMInputData)
  PutGBMRho[i] <- GBMOptionRho(GBMInputData)
  BINInputData$Type = 1
  CallEuropeanBinomialValue[i] <- ESBINOptionValue(BINInputData)
  CallEuropeanBinomialDelta[i] <- ESBINOptionDeltaDirectEnh(BINInputData)
  CallEuropeanBinomialGamma[i] <- ESBINOptionGammaDirectEnh(BINInputData)
  CallEuropeanBinomialTheta[i] <- ESBINOptionThetaDirectEnh(BINInputData)
  CallEuropeanBinomialVega[i] <- ESBINOptionVega(BINInputData)
  CallEuropeanBinomialRho[i] <- ESBINOptionRho(BINInputData)
  CallAmericanBinomialValue[i] <- ASBINOptionValue(BINInputData)
  CallAmericanBinomialDelta[i] <- ASBINOptionDeltaDirectEnh(BINInputData)
  CallAmericanBinomialGamma[i] <- ASBINOptionGammaDirectEnh(BINInputData)
  CallAmericanBinomialTheta[i] <- ASBINOptionThetaDirectEnh(BINInputData)
  CallAmericanBinomialVega[i] <- ASBINOptionVega(BINInputData)
  CallAmericanBinomialRho[i] <- ASBINOptionRho(BINInputData)
  BINInputData$Type = -1
  PutEuropeanBinomialValue[i] <- ESBINOptionValue(BINInputData)
  PutEuropeanBinomialDelta[i] <- ESBINOptionDeltaDirectEnh(BINInputData)
  PutEuropeanBinomialGamma[i] <- ESBINOptionGammaDirectEnh(BINInputData)
  PutEuropeanBinomialTheta[i] <- ESBINOptionThetaDirectEnh(BINInputData)
  PutEuropeanBinomialVega[i] <- ESBINOptionVega(BINInputData)
  PutEuropeanBinomialRho[i] <- ESBINOptionRho(BINInputData)
  PutAmericanBinomialValue[i] <- ASBINOptionValue(BINInputData)
  PutAmericanBinomialDelta[i] <- ASBINOptionDeltaDirectEnh(BINInputData)
  PutAmericanBinomialGamma[i] <- ASBINOptionGammaDirectEnh(BINInputData)
  PutAmericanBinomialTheta[i] <- ASBINOptionThetaDirectEnh(BINInputData)
  PutAmericanBinomialVega[i] <- ASBINOptionVega(BINInputData)
  PutAmericanBinomialRho[i] <- ASBINOptionRho(BINInputData)
}
# Reset volatility
GBMInputData$TimeToMaturity = inputTimeToMaturity
BINInputData$TimeToMaturity = inputTimeToMaturity
# Common to all plots
MaxValue = max(TimeToMaturity)
MinValue = min(TimeToMaturity)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
# Footer
TSU = paste0('S=', inputStockPrice)
TXU = paste0(',X=', inputStrikePrice)
TR = paste0(',r=', inputInterestRate)
TSig = paste0(',Vol=', inputVolatility)
Td = paste0(',d=', inputDividendYield)
TN = paste0(',N=', inputNumberOfSteps)
sTitle = paste0(TSU,TXU,TR,TSig,Td,TN)
lTitle = "Parameter"
#
# GBM Value Plots
#
MaxValue = max(CallGBMValue, PutGBMValue)
MinValue = min(CallGBMValue, PutGBMValue)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Value","Put Value")
mTitle = "GBMOVM Value"
xTitle = "Time To Maturity"
yTitle = "GBMOVM Value"
plot(TimeToMaturity, CallGBMValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutGBMValue, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(0, 0), 
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
#
# GBM Value Plots (ES-closed form and AS value)
#
MaxValue = max(CallGBMValue, PutGBMValue, CallAmericanBinomialValue, 
  PutAmericanBinomialValue)
MinValue = min(CallGBMValue, PutGBMValue, CallAmericanBinomialValue, 
  PutAmericanBinomialValue)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("ES Call Value","ES Put Value", "AS Call Value","AS Put Value")
mTitle = "GBMOVM Value-European-style and American-Style"
xTitle = "Time To Maturity"
yTitle = "GBMOVM Value"
plot(TimeToMaturity, CallGBMValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutGBMValue, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1, 1, 1), lty = c(0, 0, 0, 0), 
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
#
# Difference between GBM and European-style Binomial models
#
CallDifference = CallEuropeanBinomialValue - CallGBMValue
PutDifference = PutEuropeanBinomialValue - PutGBMValue
# Binomial difference plots
MaxValue = max(CallDifference, PutDifference)
MinValue = min(CallDifference, PutDifference)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Diff: ES Bin - GBM","Put Diff: ES Bin - GBM")
mTitle = "GBM to Binomial Option Value Difference"
xTitle = "Time To Maturity"
yTitle = "Difference"
plot(TimeToMaturity, CallDifference, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutDifference, type = "p", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(0, 0), bty = "n",
  col = c("black","black"), pch = c(1, 2),  title = lTitle)
#
# GBM Delta Plots
#
MaxValue = max(CallGBMDelta, CallAmericanBinomialDelta, 
  PutGBMDelta, PutAmericanBinomialDelta)
MinValue = min(CallGBMDelta, CallAmericanBinomialDelta, 
  PutGBMDelta, PutAmericanBinomialDelta)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Delta", "AS Call Delta","Put Delta", "AS Put Delta")
mTitle = "GBMOVM and Binomial Option Delta"
xTitle = "Time To Maturity"
yTitle = "Delta"
plot(TimeToMaturity, CallGBMDelta, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialDelta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, PutGBMDelta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialDelta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("left", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
#
# GBM Gamma Plots
#
MaxValue = max(CallGBMGamma, CallAmericanBinomialGamma, 
  PutGBMGamma, PutAmericanBinomialGamma)
MinValue = min(CallGBMGamma, CallAmericanBinomialGamma, 
  PutGBMGamma, PutAmericanBinomialGamma)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Gamma", "AS Call Gamma","Put Gamma", "AS Put Gamma")
mTitle = "GBMOVM and Binomial Option Gamma"
xTitle = "Time To Maturity"
yTitle = "Gamma"
plot(TimeToMaturity, CallGBMGamma, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialGamma, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, PutGBMGamma, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialGamma, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
#
# GBM Theta Plots
#
MaxValue = max(CallGBMTheta, CallAmericanBinomialTheta, 
  PutGBMTheta, PutAmericanBinomialTheta)
MinValue = min(CallGBMTheta, CallAmericanBinomialTheta, 
  PutGBMTheta, PutAmericanBinomialTheta)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Theta", "AS Call Theta","Put Theta", "AS Put Theta")
mTitle = "GBMOVM and Binomial Option Theta"
xTitle = "Time To Maturity"
yTitle = "Theta"
plot(TimeToMaturity, CallGBMTheta, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialTheta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, PutGBMTheta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialTheta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
#
# GBM Vega Plots
#
MaxValue = max(CallGBMVega, CallAmericanBinomialVega, 
  PutGBMVega, PutAmericanBinomialVega)
MinValue = min(CallGBMVega, CallAmericanBinomialVega, 
  PutGBMVega, PutAmericanBinomialVega)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Vega", "AS Call Vega","Put Vega", "AS Put Vega")
mTitle = "GBMOVM and Binomial Option Vega"
xTitle = "Time To Maturity"
yTitle = "Vega"
plot(TimeToMaturity, CallGBMVega, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialVega, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, PutGBMVega, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialVega, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
#
# GBM Rho Plots
#
MaxValue = max(CallGBMRho, CallAmericanBinomialRho, 
  PutGBMRho, PutAmericanBinomialRho)
MinValue = min(CallGBMRho, CallAmericanBinomialRho, 
  PutGBMRho, PutAmericanBinomialRho)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Rho", "AS Call Rho","Put Rho", "AS Put Rho")
mTitle = "GBMOVM and Binomial Option Rho"
xTitle = "Time To Maturity"
yTitle = "Rho"
plot(TimeToMaturity, CallGBMRho, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialRho, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, PutGBMRho, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialRho, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)


# 
# # GBM Value Plots
# MaxValue = max(CallGBMValue, PutGBMValue)
# MinValue = min(CallGBMValue, PutGBMValue)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("GBM Call Value","GBM Put Value")
# mTitle = "GBM Option Value"
# xTitle = "Time To Maturity"
# yTitle = "GBM Option Value"
# plot(TimeToMaturity, CallGBMValue, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TimeToMaturity, PutGBMValue, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
#   pch = 2, cex = 0.5)
# legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1), 
#   col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
# # Binomial Value Plots
# MaxValue = max(CallEuropeanBinomialValue, PutEuropeanBinomialValue, 
#   CallAmericanBinomialValue, PutAmericanBinomialValue)
# MinValue = min(CallEuropeanBinomialValue, PutEuropeanBinomialValue, 
#   CallAmericanBinomialValue, PutAmericanBinomialValue)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("ES Bin Call Value","ES Bin Put Value",
#   "AS Bin Call Value","AS Bin Put Value")
# mTitle = "Option Value"
# xTitle = "Time To Maturity"
# yTitle = "Option Value"
# plot(TimeToMaturity, CallEuropeanBinomialValue, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TimeToMaturity, PutEuropeanBinomialValue, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TimeToMaturity, CallAmericanBinomialValue, type = "b", col = "black", 
#   xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
# lines(TimeToMaturity, PutAmericanBinomialValue, type = "b", col ="black", 
#   xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), bty = "n",
#   col = c("black","black","black","black"), pch = c(1, 2, 3, 4), title = lTitle)
# # Difference between GBM and European-style Binomial models
# CallDifference = CallEuropeanBinomialValue - CallGBMValue
# PutDifference = PutEuropeanBinomialValue - PutGBMValue
# # Binomial difference plots
# MaxValue = max(CallDifference, PutDifference)
# MinValue = min(CallDifference, PutDifference)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("Call Diff: ES Bin - GBM","Put Diff: ES Bin - GBM")
# mTitle = "GBM to Binomial Option Value Difference"
# xTitle = "Time To Maturity"
# yTitle = "Difference"
# plot(TimeToMaturity, CallDifference, type = "p", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TimeToMaturity, PutDifference, type = "p", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("bottomleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(1,1), bty = "n",
#   col = c("black","black","black","black"), pch = c(1, 2),  title = lTitle)
# # Difference in Binomial models
# CallDifference = CallAmericanBinomialValue - CallEuropeanBinomialValue
# PutDifference = PutAmericanBinomialValue - PutEuropeanBinomialValue
# # Binomial difference plots
# MaxValue = max(CallDifference, PutDifference)
# MinValue = min(CallDifference, PutDifference)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("Call Diff: AS - ES",
#   "Put Diff: American - ES")
# mTitle = "Binomial Option Value Difference"
# xTitle = "Time To Maturity"
# yTitle = "Difference"
# plot(TimeToMaturity, CallDifference, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TimeToMaturity, PutDifference, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("right", legtxt, cex = 0.75, lwd = c(1,1), lty = c(1,1), bty = "n", 
#   col = c("black","black","black","black"), pch = c(1, 2), title = lTitle)
# # Binomial Delta Plots
# MaxValue = max(CallEuropeanBinomialDelta, PutEuropeanBinomialDelta, 
#   CallAmericanBinomialDelta, PutAmericanBinomialDelta)
# MinValue = min(CallEuropeanBinomialDelta, PutEuropeanBinomialDelta, 
#   CallAmericanBinomialDelta, PutAmericanBinomialDelta)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("ES Bin Call Delta","ES Bin Put Delta",
#   "AS Bin Call Delta","AS Bin Put Delta")
# mTitle = "Option Delta"
# xTitle = "Time To Maturity"
# yTitle = "Option Delta"
# plot(TimeToMaturity, CallEuropeanBinomialDelta, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TimeToMaturity, PutEuropeanBinomialDelta, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TimeToMaturity, CallAmericanBinomialDelta, type = "b", col = "black", 
#   xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
# lines(TimeToMaturity, PutAmericanBinomialDelta, type = "b", col ="black", 
#   xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
# legend("right", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), bty = "n",
#   col = c("black","black","black","black"), pch = c(1, 2, 3, 4), title = lTitle)
# # Binomial Gamma Plots
# MaxValue = max(CallEuropeanBinomialGamma, PutEuropeanBinomialGamma, 
#   CallAmericanBinomialGamma, PutAmericanBinomialGamma)
# MinValue = min(CallEuropeanBinomialGamma, PutEuropeanBinomialGamma, 
#   CallAmericanBinomialGamma, PutAmericanBinomialGamma)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("ES Bin Call Gamma","ES Bin Put Gamma",
#   "AS Bin Call Gamma","AS Bin Put Gamma")
# mTitle = "Option Gamma"
# xTitle = "Time To Maturity"
# yTitle = "Option Gamma"
# plot(TimeToMaturity, CallEuropeanBinomialGamma, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TimeToMaturity, PutEuropeanBinomialGamma, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TimeToMaturity, CallAmericanBinomialGamma, type = "b", col = "black", 
#   xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
# lines(TimeToMaturity, PutAmericanBinomialGamma, type = "b", col ="black", 
#   xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
# legend("right", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), bty = "n",
#   col = c("black","black","black","black"), pch = c(1, 2, 3, 4), title = lTitle)
# # Binomial Theta Plots
# MaxValue = max(CallEuropeanBinomialTheta, PutEuropeanBinomialTheta, 
#   CallAmericanBinomialTheta, PutAmericanBinomialTheta)
# MinValue = min(CallEuropeanBinomialTheta, PutEuropeanBinomialTheta, 
#   CallAmericanBinomialTheta, PutAmericanBinomialTheta)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("ES Bin Call Theta","ES Bin Put Theta",
#   "AS Bin Call Theta","AS Bin Put Theta")
# mTitle = "Option Theta"
# xTitle = "Time To Maturity"
# yTitle = "Option Theta"
# plot(TimeToMaturity, CallEuropeanBinomialTheta, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TimeToMaturity, PutEuropeanBinomialTheta, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TimeToMaturity, CallAmericanBinomialTheta, type = "b", col = "black", 
#   xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
# lines(TimeToMaturity, PutAmericanBinomialTheta, type = "b", col ="black", 
#   xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
# legend("bottomleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
#   col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
#   title = lTitle)
# # Binomial Vega Plots
# MaxValue = max(CallEuropeanBinomialVega, PutEuropeanBinomialVega, 
#   CallAmericanBinomialVega, PutAmericanBinomialVega)
# MinValue = min(CallEuropeanBinomialVega, PutEuropeanBinomialVega, 
#   CallAmericanBinomialVega, PutAmericanBinomialVega)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("ES Bin Call Vega","ES Bin Put Vega",
#   "AS Bin Call Vega","AS Bin Put Vega")
# mTitle = "Option Vega"
# xTitle = "Time To Maturity"
# yTitle = "Option Vega"
# plot(TimeToMaturity, CallEuropeanBinomialVega, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TimeToMaturity, PutEuropeanBinomialVega, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TimeToMaturity, CallAmericanBinomialVega, type = "b", col = "black", 
#   xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
# lines(TimeToMaturity, PutAmericanBinomialVega, type = "b", col ="black", 
#   xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
# legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), bty = "n", 
#   col = c("black","black","black","black"), pch = c(1, 2, 3, 4), title = lTitle)
# # Binomial Rho Plots
# MaxValue = max(CallEuropeanBinomialRho, PutEuropeanBinomialRho, 
#   CallAmericanBinomialRho, PutAmericanBinomialRho)
# MinValue = min(CallEuropeanBinomialRho, PutEuropeanBinomialRho, 
#   CallAmericanBinomialRho, PutAmericanBinomialRho)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("ES Bin Call Rho","ES Bin Put Rho",
#   "AS Bin Call Rho","AS Bin Put Rho")
# mTitle = "Option Rho"
# xTitle = "Time To Maturity"
# yTitle = "Option Rho"
# plot(TimeToMaturity, CallEuropeanBinomialRho, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TimeToMaturity, PutEuropeanBinomialRho, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TimeToMaturity, CallAmericanBinomialRho, type = "b", col = "black", 
#   xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
# lines(TimeToMaturity, PutAmericanBinomialRho, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 4, cex = 0.5)
# legend("topright", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), bty = "n", 
#   col = c("black","black","black","black"), pch = c(1, 2, 3, 4), title = lTitle)
