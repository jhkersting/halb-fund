# Time To Maturity Plots.R
#   Sub program for Module 5.4
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
  CallEuropeanBinomialValue[i] <- ESBINOptionValue(BINInputData, DIVInputData)
  CallEuropeanBinomialDelta[i] <- ESBINOptionDeltaDirect(BINInputData, DIVInputData)
  CallEuropeanBinomialGamma[i] <- ESBINOptionGammaDirect(BINInputData, DIVInputData)
  CallEuropeanBinomialTheta[i] <- ESBINOptionTheta(BINInputData, DIVInputData)
  CallEuropeanBinomialVega[i] <- ESBINOptionVega(BINInputData, DIVInputData)
  CallEuropeanBinomialRho[i] <- ESBINOptionRho(BINInputData, DIVInputData)
  CallAmericanBinomialValue[i] <- ASBINOptionValue(BINInputData, DIVInputData)
  CallAmericanBinomialDelta[i] <- ASBINOptionDeltaDirect(BINInputData, DIVInputData)
  CallAmericanBinomialGamma[i] <- ASBINOptionGammaDirect(BINInputData, DIVInputData)
  CallAmericanBinomialTheta[i] <- ASBINOptionTheta(BINInputData, DIVInputData)
  CallAmericanBinomialVega[i] <- ASBINOptionVega(BINInputData, DIVInputData)
  CallAmericanBinomialRho[i] <- ASBINOptionRho(BINInputData, DIVInputData)
  BINInputData$Type = -1
  PutEuropeanBinomialValue[i] <- ESBINOptionValue(BINInputData, DIVInputData)
  PutEuropeanBinomialDelta[i] <- ESBINOptionDeltaDirect(BINInputData, DIVInputData)
  PutEuropeanBinomialGamma[i] <- ESBINOptionGammaDirect(BINInputData, DIVInputData)
  PutEuropeanBinomialTheta[i] <- ESBINOptionTheta(BINInputData, DIVInputData)
  PutEuropeanBinomialVega[i] <- ESBINOptionVega(BINInputData, DIVInputData)
  PutEuropeanBinomialRho[i] <- ESBINOptionRho(BINInputData, DIVInputData)
  PutAmericanBinomialValue[i] <- ASBINOptionValue(BINInputData, DIVInputData)
  PutAmericanBinomialDelta[i] <- ASBINOptionDeltaDirect(BINInputData, DIVInputData)
  PutAmericanBinomialGamma[i] <- ASBINOptionGammaDirect(BINInputData, DIVInputData)
  PutAmericanBinomialTheta[i] <- ASBINOptionTheta(BINInputData, DIVInputData)
  PutAmericanBinomialVega[i] <- ASBINOptionVega(BINInputData, DIVInputData)
  PutAmericanBinomialRho[i] <- ASBINOptionRho(BINInputData, DIVInputData)
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
TXU = paste0('X=', inputStrikePrice)
TR = paste0(',r=', inputInterestRate)
TTM = paste0(',T=', inputTimeToMaturity)
TSig = paste0(',Vol=', inputVolatility)
Td = paste0(',d=', inputDividendYield)
TN = paste0(',N=', inputNumberOfSteps)
sTitle = paste0(TXU,TR,TTM,TSig,Td,TN)
lTitle = "Parameter"
# GBM Value Plots
MaxValue = max(CallGBMValue, PutGBMValue)
MinValue = min(CallGBMValue, PutGBMValue)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("GBM Call Value","GBM Put Value")
mTitle = "GBM Option Value"
xTitle = "Time To Maturity"
yTitle = "GBM Option Value"
plot(TimeToMaturity, CallGBMValue, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutGBMValue, type = "l", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1), 
  col = c("black","black"), pch = c(NA,NA), bty = "n", title = lTitle)
# Binomial Value Plots
MaxValue = max(CallEuropeanBinomialValue, PutEuropeanBinomialValue, 
  CallAmericanBinomialValue, PutAmericanBinomialValue)
MinValue = min(CallEuropeanBinomialValue, PutEuropeanBinomialValue, 
  CallAmericanBinomialValue, PutAmericanBinomialValue)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("ES Bin Call Value","ES Bin Put Value",
  "AS Bin Call Value","AS Bin Put Value")
mTitle = "Option Value"
xTitle = "Time To Maturity"
yTitle = "Option Value"
plot(TimeToMaturity, CallEuropeanBinomialValue, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutEuropeanBinomialValue, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialValue, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialValue, type = "l", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), bty = "n",
  col = c("black","black","black","black"), pch = c(NA,NA,NA,NA), title = lTitle)
# Difference between GBM and European-style Binomial models
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
legend("bottomleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(1,1), bty = "n",
  col = c("black","black","black","black"), pch = c(NA,NA),  title = lTitle)
# Difference in Binomial models
CallDifference = CallAmericanBinomialValue - CallEuropeanBinomialValue
PutDifference = PutAmericanBinomialValue - PutEuropeanBinomialValue
# Binomial difference plots
MaxValue = max(CallDifference, PutDifference)
MinValue = min(CallDifference, PutDifference)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Diff: AS - ES",
  "Put Diff: American - ES")
mTitle = "Binomial Option Value Difference"
xTitle = "Time To Maturity"
yTitle = "Difference"
plot(TimeToMaturity, CallDifference, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutDifference, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("right", legtxt, cex = 0.75, lwd = c(1,1), lty = c(1,1), bty = "n", 
  col = c("black","black","black","black"), pch = c(NA,NA), title = lTitle)
# Binomial Delta Plots
MaxValue = max(CallEuropeanBinomialDelta, PutEuropeanBinomialDelta, 
  CallAmericanBinomialDelta, PutAmericanBinomialDelta)
MinValue = min(CallEuropeanBinomialDelta, PutEuropeanBinomialDelta, 
  CallAmericanBinomialDelta, PutAmericanBinomialDelta)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("ES Bin Call Delta","ES Bin Put Delta",
  "AS Bin Call Delta","AS Bin Put Delta")
mTitle = "Option Delta"
xTitle = "Time To Maturity"
yTitle = "Option Delta"
plot(TimeToMaturity, CallEuropeanBinomialDelta, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutEuropeanBinomialDelta, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialDelta, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialDelta, type = "l", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("right", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), bty = "n",
  col = c("black","black","black","black"), pch = c(NA,NA,NA,NA), title = lTitle)
# Binomial Gamma Plots
MaxValue = max(CallEuropeanBinomialGamma, PutEuropeanBinomialGamma, 
  CallAmericanBinomialGamma, PutAmericanBinomialGamma)
MinValue = min(CallEuropeanBinomialGamma, PutEuropeanBinomialGamma, 
  CallAmericanBinomialGamma, PutAmericanBinomialGamma)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("ES Bin Call Gamma","ES Bin Put Gamma",
  "AS Bin Call Gamma","AS Bin Put Gamma")
mTitle = "Option Gamma"
xTitle = "Time To Maturity"
yTitle = "Option Gamma"
plot(TimeToMaturity, CallEuropeanBinomialGamma, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutEuropeanBinomialGamma, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialGamma, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialGamma, type = "l", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("right", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), bty = "n",
  col = c("black","black","black","black"), pch = c(NA,NA,NA,NA), title = lTitle)
# Binomial Theta Plots
MaxValue = max(CallEuropeanBinomialTheta, PutEuropeanBinomialTheta, 
  CallAmericanBinomialTheta, PutAmericanBinomialTheta)
MinValue = min(CallEuropeanBinomialTheta, PutEuropeanBinomialTheta, 
  CallAmericanBinomialTheta, PutAmericanBinomialTheta)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("ES Bin Call Theta","ES Bin Put Theta",
  "AS Bin Call Theta","AS Bin Put Theta")
mTitle = "Option Theta"
xTitle = "Time To Maturity"
yTitle = "Option Theta"
plot(TimeToMaturity, CallEuropeanBinomialTheta, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutEuropeanBinomialTheta, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialTheta, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialTheta, type = "l", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("bottomleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), 
  col = c("black","black","black","black"), pch = c(NA,NA,NA,NA), bty = "n", 
  title = lTitle)
# Binomial Vega Plots
MaxValue = max(CallEuropeanBinomialVega, PutEuropeanBinomialVega, 
  CallAmericanBinomialVega, PutAmericanBinomialVega)
MinValue = min(CallEuropeanBinomialVega, PutEuropeanBinomialVega, 
  CallAmericanBinomialVega, PutAmericanBinomialVega)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("ES Bin Call Vega","ES Bin Put Vega",
  "AS Bin Call Vega","AS Bin Put Vega")
mTitle = "Option Vega"
xTitle = "Time To Maturity"
yTitle = "Option Vega"
plot(TimeToMaturity, CallEuropeanBinomialVega, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutEuropeanBinomialVega, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialVega, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialVega, type = "l", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), bty = "n", 
  col = c("black","black","black","black"), pch = c(NA,NA,NA,NA), title = lTitle)
# Binomial Rho Plots
MaxValue = max(CallEuropeanBinomialRho, PutEuropeanBinomialRho, 
  CallAmericanBinomialRho, PutAmericanBinomialRho)
MinValue = min(CallEuropeanBinomialRho, PutEuropeanBinomialRho, 
  CallAmericanBinomialRho, PutAmericanBinomialRho)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("ES Bin Call Rho","ES Bin Put Rho",
  "AS Bin Call Rho","AS Bin Put Rho")
mTitle = "Option Rho"
xTitle = "Time To Maturity"
yTitle = "Option Rho"
plot(TimeToMaturity, CallEuropeanBinomialRho, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TimeToMaturity, PutEuropeanBinomialRho, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TimeToMaturity, CallAmericanBinomialRho, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(TimeToMaturity, PutAmericanBinomialRho, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 4, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), bty = "n", 
  col = c("black","black","black","black"), pch = c(NA,NA,NA,NA), title = lTitle)
