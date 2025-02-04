# ABMOVM Time To Maturity Plots.R
#   Sub program for Module 9.3
# Plots with StockPrice
StepSize = (UpperBoundT - LowerBoundT)/(NumberOfObservations - 1)
TimeToMaturity <- c(1:NumberOfObservations)
CallABMValue <- c(1:NumberOfObservations)
PutABMValue <- c(1:NumberOfObservations)
CallEuropeanBinomialValue <- c(1:NumberOfObservations)
PutEuropeanBinomialValue <- c(1:NumberOfObservations)
CallAmericanBinomialValue <- c(1:NumberOfObservations)
PutAmericanBinomialValue <- c(1:NumberOfObservations)
CallABMDelta <- c(1:NumberOfObservations)
PutABMDelta <- c(1:NumberOfObservations)
CallEuropeanBinomialDelta <- c(1:NumberOfObservations)
PutEuropeanBinomialDelta <- c(1:NumberOfObservations)
CallAmericanBinomialDelta <- c(1:NumberOfObservations)
PutAmericanBinomialDelta <- c(1:NumberOfObservations)
CallABMGamma <- c(1:NumberOfObservations)
PutABMGamma <- c(1:NumberOfObservations)
CallEuropeanBinomialGamma <- c(1:NumberOfObservations)
PutEuropeanBinomialGamma <- c(1:NumberOfObservations)
CallAmericanBinomialGamma <- c(1:NumberOfObservations)
PutAmericanBinomialGamma <- c(1:NumberOfObservations)
CallABMTheta <- c(1:NumberOfObservations)
PutABMTheta <- c(1:NumberOfObservations)
CallEuropeanBinomialTheta <- c(1:NumberOfObservations)
PutEuropeanBinomialTheta <- c(1:NumberOfObservations)
CallAmericanBinomialTheta <- c(1:NumberOfObservations)
PutAmericanBinomialTheta <- c(1:NumberOfObservations)
CallABMVega <- c(1:NumberOfObservations)
PutABMVega <- c(1:NumberOfObservations)
CallEuropeanBinomialVega <- c(1:NumberOfObservations)
PutEuropeanBinomialVega <- c(1:NumberOfObservations)
CallAmericanBinomialVega <- c(1:NumberOfObservations)
PutAmericanBinomialVega <- c(1:NumberOfObservations)
CallABMRho <- c(1:NumberOfObservations)
PutABMRho <- c(1:NumberOfObservations)
CallEuropeanBinomialRho <- c(1:NumberOfObservations)
PutEuropeanBinomialRho <- c(1:NumberOfObservations)
CallAmericanBinomialRho <- c(1:NumberOfObservations)
PutAmericanBinomialRho <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  TimeToMaturity[i] <- LowerBoundT + (i - 1)*StepSize
  ABMInputData$TimeToMaturity = TimeToMaturity[i]
  BINInputData$TimeToMaturity = TimeToMaturity[i]
  ABMInputData$Type = 1
  CallABMValue[i] <- ABMOptionValue(ABMInputData)
  CallABMDelta[i] <- ABMOptionDelta(ABMInputData)
  CallABMGamma[i] <- ABMOptionGamma(ABMInputData)
  CallABMVega[i] <- ABMOptionVega(ABMInputData)
  CallABMTheta[i] <- ABMOptionTheta(ABMInputData)
  CallABMRho[i] <- ABMOptionRho(ABMInputData)
  ABMInputData$Type = -1
  PutABMValue[i] <- ABMOptionValue(ABMInputData)
  PutABMDelta[i] <- ABMOptionDelta(ABMInputData)
  PutABMGamma[i] <- ABMOptionGamma(ABMInputData)
  PutABMVega[i] <- ABMOptionVega(ABMInputData)
  PutABMTheta[i] <- ABMOptionTheta(ABMInputData)
  PutABMRho[i] <- ABMOptionRho(ABMInputData)
  ABMInputData$Type = 1 # Reset
  Value <- ABMESBINOptionValue(BINInputData)
  CallEuropeanBinomialValue[i] = Value$CallValue
  PutEuropeanBinomialValue[i] = Value$PutValue
  Delta <- ABMESBINOptionDeltaDirectEnh(BINInputData)
  CallEuropeanBinomialDelta[i] = Delta$ESCallDelta
  PutEuropeanBinomialDelta[i] = Delta$ESPutDelta
  Gamma <- ABMESBINOptionGammaDirectEnh(BINInputData)
  CallEuropeanBinomialGamma[i] = Gamma$ESCallGamma
  PutEuropeanBinomialGamma[i] = Gamma$ESPutGamma
  Theta <- ABMESBINOptionThetaDirectEnh(BINInputData)
  CallEuropeanBinomialTheta[i] = Theta$ESCallTheta
  PutEuropeanBinomialTheta[i] = Theta$ESPutTheta
  Vega <- ABMESBINOptionNVega(BINInputData)
  CallEuropeanBinomialVega[i] = Vega$ESCallVega
  PutEuropeanBinomialVega[i] = Vega$ESPutVega
  Rho <- ABMESBINOptionNRho(BINInputData)
  CallEuropeanBinomialRho[i] = Rho$ESCallRho
  PutEuropeanBinomialRho[i] = Rho$ESPutRho
  Value <- ABMASOptionValue(BINInputData)
  CallAmericanBinomialValue[i] = Value$CallValue
  PutAmericanBinomialValue[i] = Value$PutValue
  Delta <- ABMASBINOptionDeltaDirectEnh(BINInputData)
  CallAmericanBinomialDelta[i] = Delta$ASCallDelta
  PutAmericanBinomialDelta[i] = Delta$ASPutDelta
  Gamma <- ABMASBINOptionGammaDirectEnh(BINInputData)
  CallAmericanBinomialGamma[i] = Gamma$ASCallGamma
  PutAmericanBinomialGamma[i] = Gamma$ASPutGamma
  Theta <- ABMASBINOptionThetaDirectEnh(BINInputData)
  CallAmericanBinomialTheta[i] = Theta$ASCallTheta
  PutAmericanBinomialTheta[i] = Theta$ASPutTheta
  Vega <- ABMASBINOptionNVega(BINInputData)
  CallAmericanBinomialVega[i] = Vega$ASCallVega
  PutAmericanBinomialVega[i] = Vega$ASPutVega
  Rho <- ABMASBINOptionNRho(BINInputData)
  CallAmericanBinomialRho[i] = Rho$ASCallRho
  PutAmericanBinomialRho[i] = Rho$ASPutRho
}
# Reset volatility
ABMInputData$TimeToMaturity = inputTimeToMaturity
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
TSig = paste0(',Vol=', round(inputVolatility, 2))
Td = paste0(',d=', inputDividendYield)
TN = paste0(',N=', inputNumberOfSteps)
sTitle = paste0(TSU,TXU,TR,TSig,Td,TN)
lTitle = "Parameter"
#
# ABM Value Plots
#
MaxValue = max(CallABMValue, PutABMValue)
MinValue = min(CallABMValue, PutABMValue)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Value","Put Value")
mTitle = "ABMOVM Value"
xTitle = "Time To Maturity"
yTitle = "ABMOVM Value"
plot(TimeToMaturity, CallABMValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutABMValue, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(0, 0), 
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
#
# ABM Value Plots (ES-closed form and AS value)
#
MaxValue = max(CallABMValue, PutABMValue, CallAmericanBinomialValue, 
  PutAmericanBinomialValue)
MinValue = min(CallABMValue, PutABMValue, CallAmericanBinomialValue, 
  PutAmericanBinomialValue)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("ES Call Value","ES Put Value", "AS Call Value","AS Put Value")
mTitle = "ABMOVM Value-European-style and American-Style"
xTitle = "Time To Maturity"
yTitle = "ABMOVM Value"
plot(TimeToMaturity, CallABMValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutABMValue, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1, 1, 1), lty = c(0, 0, 0, 0), 
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
#
# Difference between ABM and European-style Binomial models
#
CallDifference = CallEuropeanBinomialValue - CallABMValue
PutDifference = PutEuropeanBinomialValue - PutABMValue
# Binomial difference plots
MaxValue = max(CallDifference, PutDifference)
MinValue = min(CallDifference, PutDifference)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Diff: ES Bin - ABM","Put Diff: ES Bin - ABM")
mTitle = "ABM to Binomial Option Value Difference"
xTitle = "Time To Maturity"
yTitle = "Difference"
plot(TimeToMaturity, CallDifference, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutDifference, type = "p", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1,1), lty = c(0, 0), bty = "n",
  col = c("black","black"), pch = c(1, 2),  title = lTitle)
#
# ABM Delta Plots
#
MaxValue = max(CallABMDelta, CallAmericanBinomialDelta, 
  PutABMDelta, PutAmericanBinomialDelta)
MinValue = min(CallABMDelta, CallAmericanBinomialDelta, 
  PutABMDelta, PutAmericanBinomialDelta)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Delta", "AS Call Delta","Put Delta", "AS Put Delta")
mTitle = "ABMOVM and Binomial Option Delta"
xTitle = "Time To Maturity"
yTitle = "Delta"
plot(TimeToMaturity, CallABMDelta, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialDelta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, PutABMDelta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialDelta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("left", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
#
# ABM Gamma Plots
#
MaxValue = max(CallABMGamma, CallAmericanBinomialGamma, 
  PutABMGamma, PutAmericanBinomialGamma)
MinValue = min(CallABMGamma, CallAmericanBinomialGamma, 
  PutABMGamma, PutAmericanBinomialGamma)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Gamma", "AS Call Gamma","Put Gamma", "AS Put Gamma")
mTitle = "ABMOVM and Binomial Option Gamma"
xTitle = "Time To Maturity"
yTitle = "Gamma"
plot(TimeToMaturity, CallABMGamma, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialGamma, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, PutABMGamma, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialGamma, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
#
# ABM Theta Plots
#
MaxValue = max(CallABMTheta, CallAmericanBinomialTheta, 
  PutABMTheta, PutAmericanBinomialTheta)
MinValue = min(CallABMTheta, CallAmericanBinomialTheta, 
  PutABMTheta, PutAmericanBinomialTheta)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Theta", "AS Call Theta","Put Theta", "AS Put Theta")
mTitle = "ABMOVM and Binomial Option Theta"
xTitle = "Time To Maturity"
yTitle = "Theta"
plot(TimeToMaturity, CallABMTheta, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialTheta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, PutABMTheta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialTheta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
#
# ABM Vega Plots
#
MaxValue = max(CallABMVega, CallAmericanBinomialVega, 
  PutABMVega, PutAmericanBinomialVega)
MinValue = min(CallABMVega, CallAmericanBinomialVega, 
  PutABMVega, PutAmericanBinomialVega)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Vega", "AS Call Vega","Put Vega", "AS Put Vega")
mTitle = "ABMOVM and Binomial Option Vega"
xTitle = "Time To Maturity"
yTitle = "Vega"
plot(TimeToMaturity, CallABMVega, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialVega, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, PutABMVega, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialVega, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
#
# ABM Rho Plots
#
MaxValue = max(CallABMRho, CallAmericanBinomialRho, 
  PutABMRho, PutAmericanBinomialRho)
MinValue = min(CallABMRho, CallAmericanBinomialRho, 
  PutABMRho, PutAmericanBinomialRho)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Rho", "AS Call Rho","Put Rho", "AS Put Rho")
mTitle = "ABMOVM and Binomial Option Rho"
xTitle = "Time To Maturity"
yTitle = "Rho"
plot(TimeToMaturity, CallABMRho, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialRho, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, PutABMRho, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialRho, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)


# 
# # ABM Value Plots
# MaxValue = max(CallABMValue, PutABMValue)
# MinValue = min(CallABMValue, PutABMValue)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("ABM Call Value","ABM Put Value")
# mTitle = "ABM Option Value"
# xTitle = "Time To Maturity"
# yTitle = "ABM Option Value"
# plot(TimeToMaturity, CallABMValue, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TimeToMaturity, PutABMValue, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
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
# # Difference between ABM and European-style Binomial models
# CallDifference = CallEuropeanBinomialValue - CallABMValue
# PutDifference = PutEuropeanBinomialValue - PutABMValue
# # Binomial difference plots
# MaxValue = max(CallDifference, PutDifference)
# MinValue = min(CallDifference, PutDifference)
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("Call Diff: ES Bin - ABM","Put Diff: ES Bin - ABM")
# mTitle = "ABM to Binomial Option Value Difference"
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
