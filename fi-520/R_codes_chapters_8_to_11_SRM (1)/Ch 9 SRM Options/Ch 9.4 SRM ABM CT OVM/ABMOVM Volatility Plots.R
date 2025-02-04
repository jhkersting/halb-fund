# ABMOVM Volatility Plots.R
#   Sub program for Module 9.3
# Plots with StockPrice
StepSize = (UpperBoundV - LowerBoundV)/(NumberOfObservations - 1)
Volatility <- c(1:NumberOfObservations)
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
  Volatility[i] <-LowerBoundV + (i - 1)*StepSize
  ABMInputData$Volatility = Volatility[i]
  BINInputData$Volatility = Volatility[i]
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
ABMInputData$Volatility = inputVolatility
BINInputData$Volatility = inputVolatility
# Common to all plots
MaxValue = max(Volatility)
MinValue = min(Volatility)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
# Footer
TSU = paste0('S=', inputStockPrice)
TXU = paste0(',X=', inputStrikePrice)
TR = paste0(',r=', inputInterestRate)
TTM = paste0(',T=', inputTimeToMaturity)
Td = paste0(',d=', inputDividendYield)
TN = paste0(',N=', inputNumberOfSteps)
sTitle = paste0(TSU,TXU,TR,TTM,Td,TN)
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
xTitle = "Volatility"
yTitle = "ABMOVM Value"
plot(Volatility, CallABMValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(Volatility, PutABMValue, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1), 
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
xTitle = "Volatility"
yTitle = "ABMOVM Value"
plot(Volatility, CallABMValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(Volatility, PutABMValue, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(Volatility, CallAmericanBinomialValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(Volatility, PutAmericanBinomialValue, type = "b", col ="black", 
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
xTitle = "Volatility"
yTitle = "Difference"
plot(Volatility, CallDifference, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(Volatility, PutDifference, type = "p", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1,1), lty = c(0,0), bty = "n",
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
xTitle = "Volatility"
yTitle = "Delta"
plot(Volatility, CallABMDelta, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(Volatility, CallAmericanBinomialDelta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(Volatility, PutABMDelta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(Volatility, PutAmericanBinomialDelta, type = "b", col ="black", 
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
xTitle = "Volatility"
yTitle = "Gamma"
plot(Volatility, CallABMGamma, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(Volatility, CallAmericanBinomialGamma, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(Volatility, PutABMGamma, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(Volatility, PutAmericanBinomialGamma, type = "b", col ="black", 
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
xTitle = "Volatility"
yTitle = "Theta"
plot(Volatility, CallABMTheta, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(Volatility, CallAmericanBinomialTheta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(Volatility, PutABMTheta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(Volatility, PutAmericanBinomialTheta, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("bottomleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
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
xTitle = "Volatility"
yTitle = "Vega"
plot(Volatility, CallABMVega, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(Volatility, CallAmericanBinomialVega, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(Volatility, PutABMVega, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(Volatility, PutAmericanBinomialVega, type = "b", col ="black", 
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
xTitle = "Volatility"
yTitle = "Rho"
plot(Volatility, CallABMRho, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(Volatility, CallAmericanBinomialRho, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(Volatility, PutABMRho, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(Volatility, PutAmericanBinomialRho, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("left", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
