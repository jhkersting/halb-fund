# Stock Price Plots.R
#   Sub program for Module 5.4
# Plots with StockPrice
StepSize = (UpperBound - LowerBound)/(NumberOfObservations - 1)
StockPrice <- c(1:NumberOfObservations)
CallVLowerBound <- c(1:NumberOfObservations)
PutVLowerBound <- c(1:NumberOfObservations)
CallVUpperBound <- c(1:NumberOfObservations)
PutVUpperBound <- c(1:NumberOfObservations)
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
  StockPrice[i] <-LowerBound + (i - 1)*StepSize
  GBMInputData$StockPrice = StockPrice[i]
  BINInputData$StockPrice = StockPrice[i]
  GBMInputData$Type = 1
  CallVLowerBound[i] <- OptionLowerBound(GBMInputData)
  CallVUpperBound[i] <- OptionUpperBound(GBMInputData)
  CallGBMValue[i] <- GBMOptionValue(GBMInputData)
  CallGBMDelta[i] <- GBMOptionDelta(GBMInputData)
  CallGBMGamma[i] <- GBMOptionGamma(GBMInputData)
  CallGBMVega[i] <- GBMOptionVega(GBMInputData)
  CallGBMTheta[i] <- GBMOptionTheta(GBMInputData)
  CallGBMRho[i] <- GBMOptionRho(GBMInputData)
  GBMInputData$Type = -1
  PutVLowerBound[i] <- OptionLowerBound(GBMInputData)
  PutVUpperBound[i] <- OptionUpperBound(GBMInputData)
  PutGBMValue[i] <- GBMOptionValue(GBMInputData)
  PutGBMDelta[i] <- GBMOptionDelta(GBMInputData)
  PutGBMGamma[i] <- GBMOptionGamma(GBMInputData)
  PutGBMVega[i] <- GBMOptionVega(GBMInputData)
  PutGBMTheta[i] <- GBMOptionTheta(GBMInputData)
  PutGBMRho[i] <- GBMOptionRho(GBMInputData)
  BINInputData$Type = 1
  CallEuropeanBinomialValue[i] <- ESBINOptionValue(BINInputData)
  CallEuropeanBinomialDelta[i] <- ESBINOptionDeltaDirect(BINInputData)
  CallEuropeanBinomialGamma[i] <- ESBINOptionGammaDirect(BINInputData)
  CallEuropeanBinomialTheta[i] <- ESBINOptionTheta(BINInputData)
  CallEuropeanBinomialVega[i] <- ESBINOptionVega(BINInputData)
  CallEuropeanBinomialRho[i] <- ESBINOptionRho(BINInputData)
  CallAmericanBinomialValue[i] <- ASBINOptionValue(BINInputData)
  CallAmericanBinomialDelta[i] <- ASBINOptionDeltaDirect(BINInputData)
  CallAmericanBinomialGamma[i] <- ASBINOptionGammaDirect(BINInputData)
  CallAmericanBinomialTheta[i] <- ASBINOptionTheta(BINInputData)
  CallAmericanBinomialVega[i] <- ASBINOptionVega(BINInputData)
  CallAmericanBinomialRho[i] <- ASBINOptionRho(BINInputData)
  BINInputData$Type = -1
  PutEuropeanBinomialValue[i] <- ESBINOptionValue(BINInputData)
  PutEuropeanBinomialDelta[i] <- ESBINOptionDeltaDirect(BINInputData)
  PutEuropeanBinomialGamma[i] <- ESBINOptionGammaDirect(BINInputData)
  PutEuropeanBinomialTheta[i] <- ESBINOptionTheta(BINInputData)
  PutEuropeanBinomialVega[i] <- ESBINOptionVega(BINInputData)
  PutEuropeanBinomialRho[i] <- ESBINOptionRho(BINInputData)
  PutAmericanBinomialValue[i] <- ASBINOptionValue(BINInputData)
  PutAmericanBinomialDelta[i] <- ASBINOptionDeltaDirect(BINInputData)
  PutAmericanBinomialGamma[i] <- ASBINOptionGammaDirect(BINInputData)
  PutAmericanBinomialTheta[i] <- ASBINOptionTheta(BINInputData)
  PutAmericanBinomialVega[i] <- ASBINOptionVega(BINInputData)
  PutAmericanBinomialRho[i] <- ASBINOptionRho(BINInputData)
}
GBMInputData$StockPrice = inputStockPrice
BINInputData$StockPrice = inputStockPrice
# Common to all plots
MaxValue = max(StockPrice)
MinValue = min(StockPrice)
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
# Option Boundary Value Plots
MaxValue = max(CallVLowerBound, PutVLowerBound, CallVUpperBound, PutVUpperBound)
MinValue = min(CallVLowerBound, PutVLowerBound, CallVUpperBound, PutVUpperBound)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Lower Bound","Put Lower Bound","Call Upper Bound","Put Upper Bound")
mTitle = "Option Boundaries"
xTitle = "Stock Price"
yTitle = "Boundary Value"
plot(StockPrice, CallVLowerBound, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutVLowerBound, type = "l", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
lines(StockPrice, CallVUpperBound, type = "l", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 3, cex = 0.5)
lines(StockPrice, PutVUpperBound, type = "l", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1, 1), lty = c(1, 1, 1, 1), 
  col = c("black","black","black","black"), pch = c(NA,NA,NA,NA), bty = "n", 
  title = lTitle)
# GBM Value Plots
MaxValue = max(CallGBMValue, PutGBMValue)
MinValue = min(CallGBMValue, PutGBMValue)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("GBM Call Value","GBM Put Value")
mTitle = "GBM Option Value"
xTitle = "Stock Price"
yTitle = "GBM Option Value"
plot(StockPrice, CallGBMValue, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutGBMValue, type = "l", col ="black", xlim = xlim1, ylim = ylim1,
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
xTitle = "Stock Price"
yTitle = "Option Value"
plot(StockPrice, CallEuropeanBinomialValue, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutEuropeanBinomialValue, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CallAmericanBinomialValue, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, PutAmericanBinomialValue, type = "l", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), bty = "n",
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
xTitle = "Stock Price"
yTitle = "Difference"
plot(StockPrice, CallDifference, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutDifference, type = "p", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(1,1), bty = "n",
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
legtxt = c("Call Diff: AS - ES", "Put Diff: AS - ES")
mTitle = "Binomial Option Value Difference"
xTitle = "Stock Price"
yTitle = "Difference"
plot(StockPrice, CallDifference, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutDifference, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1), lty = c(1,1), bty = "n", 
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
xTitle = "Stock Price"
yTitle = "Option Delta"
plot(StockPrice, CallEuropeanBinomialDelta, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutEuropeanBinomialDelta, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CallAmericanBinomialDelta, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, PutAmericanBinomialDelta, type = "l", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), bty = "n",
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
xTitle = "Stock Price"
yTitle = "Option Gamma"
plot(StockPrice, CallEuropeanBinomialGamma, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutEuropeanBinomialGamma, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CallAmericanBinomialGamma, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, PutAmericanBinomialGamma, type = "l", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("bottom", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), bty = "n",
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
xTitle = "Stock Price"
yTitle = "Option Theta"
plot(StockPrice, CallEuropeanBinomialTheta, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutEuropeanBinomialTheta, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CallAmericanBinomialTheta, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, PutAmericanBinomialTheta, type = "l", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), 
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
xTitle = "Stock Price"
yTitle = "Option Vega"
plot(StockPrice, CallEuropeanBinomialVega, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutEuropeanBinomialVega, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CallAmericanBinomialVega, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, PutAmericanBinomialVega, type = "l", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), bty = "n", 
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
xTitle = "Stock Price"
yTitle = "Option Rho"
plot(StockPrice, CallEuropeanBinomialRho, type = "l", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutEuropeanBinomialRho, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CallAmericanBinomialRho, type = "l", col = "black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, PutAmericanBinomialRho, type = "l", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1), bty = "n", 
  col = c("black","black","black","black"), pch = c(NA,NA,NA,NA), title = lTitle)
