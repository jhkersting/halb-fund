# Stock Price Plots.R
# Plots with StockPrice
StepSize = (UpperBound - LowerBound)/(NumberOfObservations - 1)
StockPrice <- c(1:NumberOfObservations)
CallVLowerBound <- c(1:NumberOfObservations)
PutVLowerBound <- c(1:NumberOfObservations)
CallVUpperBound <- c(1:NumberOfObservations)
PutVUpperBound <- c(1:NumberOfObservations)
CallBSMValue <- c(1:NumberOfObservations)
PutBSMValue <- c(1:NumberOfObservations)
CallBSMDelta <- c(1:NumberOfObservations)
PutBSMDelta <- c(1:NumberOfObservations)
CallBSMGamma <- c(1:NumberOfObservations)
PutBSMGamma <- c(1:NumberOfObservations)
CallBSMTheta <- c(1:NumberOfObservations)
PutBSMTheta <- c(1:NumberOfObservations)
CallBSMVega <- c(1:NumberOfObservations)
PutBSMVega <- c(1:NumberOfObservations)
CallBSMRho <- c(1:NumberOfObservations)
PutBSMRho <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  StockPrice[i] <-LowerBound + (i - 1)*StepSize
  BSMInputData$StockPrice = StockPrice[i]
  BSMInputData$Type = 1
  CallVLowerBound[i] <- OptionLowerBound(BSMInputData)
  CallVUpperBound[i] <- OptionUpperBound(BSMInputData)
  CallBSMValue[i] <- BSMOptionValue(BSMInputData)
  CallBSMDelta[i] <- BSMOptionDelta(BSMInputData)
  CallBSMGamma[i] <- BSMOptionGamma(BSMInputData)
  CallBSMVega[i] <- BSMOptionVega(BSMInputData)
  CallBSMTheta[i] <- BSMOptionTheta(BSMInputData)
  CallBSMRho[i] <- BSMOptionRho(BSMInputData)
  BSMInputData$Type = -1
  PutVLowerBound[i] <- OptionLowerBound(BSMInputData)
  PutVUpperBound[i] <- OptionUpperBound(BSMInputData)
  PutBSMValue[i] <- BSMOptionValue(BSMInputData)
  PutBSMDelta[i] <- BSMOptionDelta(BSMInputData)
  PutBSMGamma[i] <- BSMOptionGamma(BSMInputData)
  PutBSMVega[i] <- BSMOptionVega(BSMInputData)
  PutBSMTheta[i] <- BSMOptionTheta(BSMInputData)
  PutBSMRho[i] <- BSMOptionRho(BSMInputData)
}
# NEED TO RESET B$StockPrice ???
BSMInputData$StockPrice = inputStockPrice
# Common to all plots
MaxValue = max(StockPrice)
MinValue = min(StockPrice)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
# Footer
TXU = paste0('X = ', inputStrikePrice)
TR = paste0(', r = ', inputInterestRate)
TTM = paste0(', T - t = ', inputTimeToMaturity)
TSig = paste0(', Vol = ', inputVolatility)
Td = paste0(', Div = ', inputDividendYield)
sTitle = paste0(TXU,TR,TTM,TSig,Td)
lTitle = "Parameter"
# BSM Value Plots
MaxValue = max(CallBSMValue, PutBSMValue, CallVLowerBound, PutVLowerBound)
MinValue = min(CallBSMValue, PutBSMValue, CallVLowerBound, PutVLowerBound)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("BSM Call Value","BSM Put Value", "Call Lower Bound", 
  "Put Lower Bound")
mTitle = "BSM Option Values and Lower Boundaries"
xTitle = "Stock Price"
yTitle = "Option Value"
plot(StockPrice, CallBSMValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutBSMValue, type = "p", col ="red", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CallVLowerBound, type = "p", col ="green", xlim = xlim1, 
  ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, PutVLowerBound, type = "p", col ="purple", xlim = xlim1, 
  ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0), 
  col = c("blue","red","green","purple"), pch = c(1,2,3,4), bty = "n", 
  title = lTitle)
# BSM Delta Plots
MaxValue = max(CallBSMDelta, PutBSMDelta)
MinValue = min(CallBSMDelta, PutBSMDelta)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Delta","Put Delta")
mTitle = "Option Delta"
xTitle = "Stock Price"
yTitle = "Option Delta"
plot(StockPrice, CallBSMDelta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutBSMDelta, type = "p", col ="red", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(0,0), bty = "n",
  col = c("blue","red"), pch = c(1,2), title = lTitle)
# BSM Gamma Plots
MaxValue = max(CallBSMGamma, PutBSMGamma)
MinValue = min(CallBSMGamma, PutBSMGamma)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Gamma","Put Gamma")
mTitle = "Option Gamma"
xTitle = "Stock Price"
yTitle = "Option Gamma"
plot(StockPrice, CallBSMGamma, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1, 
     ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutBSMGamma, type = "p", col ="red", xlim = xlim1, 
      ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(0,0), bty = "n",
       col = c("blue","red"), pch = c(1,2), title = lTitle)
# BSM Theta Plots
MaxValue = max(CallBSMTheta, PutBSMTheta)
MinValue = min(CallBSMTheta, PutBSMTheta)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Theta","Put Theta")
mTitle = "Option Theta"
xTitle = "Stock Price"
yTitle = "Option Theta"
plot(StockPrice, CallBSMTheta, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1, 
     ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutBSMTheta, type = "p", col ="red", xlim = xlim1, 
      ylim = ylim1, pch = 2, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1,1), lty = c(0,0), bty = "n",
       col = c("blue","red"), pch = c(1,2), title = lTitle)
# BSM Vega Plots
MaxValue = max(CallBSMVega, PutBSMVega)
MinValue = min(CallBSMVega, PutBSMVega)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Vega","Put Vega")
mTitle = "Option Vega"
xTitle = "Stock Price"
yTitle = "Option Vega"
plot(StockPrice, CallBSMVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1, 
     ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutBSMVega, type = "p", col ="red", xlim = xlim1, 
      ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(0,0),  bty = "n",
       col = c("blue","red"), pch = c(1,2), title = lTitle)
# BSM Rho Plots
MaxValue = max(CallBSMRho, PutBSMRho)
MinValue = min(CallBSMRho, PutBSMRho)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Call Rho","Put Rho")
mTitle = "Option Rho"
xTitle = "Stock Price"
yTitle = "Option Rho"
plot(StockPrice, CallBSMRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1, 
     ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutBSMRho, type = "p", col ="red", xlim = xlim1, 
      ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(0,0),  bty = "n",
       col = c("blue","red"), pch = c(1,2), title = lTitle)
