# Valuation ABM Binomial OVM American-Style Test.R
# Arithmetic Brownian Motion
# Illustrating American-style binomial option valuation and related functions
# rmarkdown::render("Valuation ABM Binomial OVM American-Style Test.R", 
#  "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
PackagesToLibrary <- c("beepr") # Libraries
if (length(setdiff(PackagesToLibrary, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(PackagesToLibrary, rownames(installed.packages())))  
} # Make sure libraries are installed on this computer
lapply(PackagesToLibrary,library,character.only = TRUE) # Load and attach libraries
rm(PackagesToLibrary)
source('ASABMBINOVM Backward Recursion With SRM Functions.R')
source('ESABMBINOVM Backward Recursion With SRM Functions.R')
# Test inputs
inputStockPrice = 100.0          # Need "input" as using variable names below
inputStrikePrice = 100.0         # In currency units, numeric
inputInterestRate = 5.0          # In percent
inputDividendYield = 5.0         # In percent
inputVolatility = 29.88476829    # 29.88476829   # In dollars, annualized
inputTimeToMaturity = 1.0        # In fraction of year
inputType = 1L                   # 1 for call, -1 for put
inputNumberOfSteps = as.integer(500)   # Or use L: 1000L
inputPayoutType = 1L             # 1 Plain vanilla, 2 digital
inputEMMProbability = 50.0       # In percent
inputDigitalPayout = 100.0
LowerBoundSP = 50 # Analysis wrt stock price
UpperBoundSP = 150
NumberOfObservations = 101
StepSize = 2L # Analysis wrt number of steps
MinStep = 2L # Must be multiple of StepSize
MaxStep = 500L # Must be multiple of StepSize
# Plot footers
TS = paste0('S=', inputStockPrice)
TX = paste0(',X=', inputStrikePrice)
TR = paste0(',r=', inputInterestRate)
Td = paste0(',d=', inputDividendYield, ',')
TV = paste0('Vol=', round(inputVolatility,4))
TT = paste0(',T=',inputTimeToMaturity)
TN = paste0(',N=',inputNumberOfSteps)
TDP = paste0(',DP=', inputDigitalPayout)
sTitleBIN = paste0(TS,TX, TR, Td, TV, TT, TN)
sTitleBINDP = paste0(TS,TX, TR, Td, TV, TT, TN, TDP)
#  BINInputData - list of inputs with associated names
BINInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
  inputDividendYield, inputVolatility, inputTimeToMaturity, inputType, 
  inputNumberOfSteps, inputPayoutType, inputEMMProbability, 
  inputDigitalPayout)
names(BINInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
  "DividendYield", "Volatility", "TimeToMaturity", "Type", "NumberOfSteps", 
  "PayoutType", "EMMProbability", "DigitalPayout")
# Values <- ABMESOptionValue(BINInputData)
# Values
BINInputData$Type <- 1
ESPlainVanillaCallLB = ESOptionLowerBound(BINInputData)
ESPlainVanillaCallUB = ESOptionUpperBound(BINInputData)
ASPlainVanillaCallLB = ASOptionLowerBound(BINInputData)
ASPlainVanillaCallUB = ASOptionUpperBound(BINInputData)
BINInputData$Type <- -1
ESPlainVanillaPutLB = ESOptionLowerBound(BINInputData)
ESPlainVanillaPutUB = ESOptionUpperBound(BINInputData)
ASPlainVanillaPutLB = ASOptionLowerBound(BINInputData)
ASPlainVanillaPutUB = ASOptionUpperBound(BINInputData)
ESPlainVanillaCallLB; ESPlainVanillaCallUB
ASPlainVanillaCallLB; ASPlainVanillaCallUB
ESPlainVanillaPutLB; ESPlainVanillaPutUB
ASPlainVanillaPutLB; ASPlainVanillaPutUB
# American-style
Values = ABMASOptionValue(BINInputData)
ASPlainVanillaCallValue <- Values$CallValue
ASPlainVanillaPutValue <- Values$PutValue
ASDigitalCallValue <- Values$DigitalCallValue
ASDigitalPutValue <- Values$DigitalPutValue
ASPlainVanillaCallValue; ASPlainVanillaPutValue 
ASDigitalCallValue; ASDigitalPutValue
# European-style
Values = ABMESOptionValue(BINInputData)
ESPlainVanillaCallValue <- Values$CallValue
ESPlainVanillaPutValue <- Values$PutValue
ESDigitalCallValue <- Values$DigitalCallValue
ESDigitalPutValue <- Values$DigitalPutValue
ESPlainVanillaCallValue; ESPlainVanillaPutValue 
ESDigitalCallValue; ESDigitalPutValue

PVCallDiff <- ASPlainVanillaCallValue - ESPlainVanillaCallValue
PVPutDiff <- ASPlainVanillaPutValue - ESPlainVanillaPutValue
DigCallDiff <- ASDigitalCallValue - ESDigitalCallValue 
DigPutDiff <- ASDigitalPutValue - ESDigitalPutValue 
PVCallDiff; PVPutDiff; DigCallDiff; DigPutDiff
#
# StockPrice Plots
#
StepSizeSP = (UpperBoundSP - LowerBoundSP)/(NumberOfObservations - 1)
StockPrice <- numeric(NumberOfObservations)
ESPlainVanillaCallLB <- numeric(NumberOfObservations)
ESPlainVanillaCallUB <- numeric(NumberOfObservations)
ESPlainVanillaPutLB <- numeric(NumberOfObservations)
ESPlainVanillaPutUB <- numeric(NumberOfObservations)
ESPlainVanillaCallValue <- numeric(NumberOfObservations)
ESPlainVanillaPutValue <- numeric(NumberOfObservations)
ESDigitalCallValue <- numeric(NumberOfObservations)
ESDigitalPutValue <- numeric(NumberOfObservations)
ASPlainVanillaCallLB <- numeric(NumberOfObservations)
ASPlainVanillaCallUB <- numeric(NumberOfObservations)
ASPlainVanillaPutLB <- numeric(NumberOfObservations)
ASPlainVanillaPutUB <- numeric(NumberOfObservations)
ASPlainVanillaCallValue <- numeric(NumberOfObservations)
ASPlainVanillaPutValue <- numeric(NumberOfObservations)
ASDigitalCallValue <- numeric(NumberOfObservations)
ASDigitalPutValue <- numeric(NumberOfObservations)
for(i in 1:NumberOfObservations){
  StockPrice[i] <-LowerBoundSP + (i - 1)*StepSizeSP
  BINInputData$StockPrice = StockPrice[i]
  BINInputData$Type = 1L
  BINInputData$PayoutType = 1L
  ESPlainVanillaCallLB[i] = ESOptionLowerBound(BINInputData)
  ESPlainVanillaCallUB[i] = ESOptionUpperBound(BINInputData)
  ASPlainVanillaCallLB[i] = ASOptionLowerBound(BINInputData)
  ASPlainVanillaCallUB[i] = ASOptionUpperBound(BINInputData)
  BINInputData$Type = -1L
  ESPlainVanillaPutLB[i] = ESOptionLowerBound(BINInputData)
  ESPlainVanillaPutUB[i] = ESOptionUpperBound(BINInputData)
  ASPlainVanillaPutLB[i] = ASOptionLowerBound(BINInputData)
  ASPlainVanillaPutUB[i] = ASOptionUpperBound(BINInputData)
  Value <- ABMESOptionValue(BINInputData)
  ESPlainVanillaCallValue[i] = Value$CallValue
  ESPlainVanillaPutValue[i] = Value$PutValue
  ESDigitalCallValue[i] = Value$DigitalCallValue
  ESDigitalPutValue[i] = Value$DigitalPutValue
  Value <- ABMASOptionValue(BINInputData)
  ASPlainVanillaCallValue[i] = Value$CallValue
  ASPlainVanillaPutValue[i] = Value$PutValue
  ASDigitalCallValue[i] = Value$DigitalCallValue
  ASDigitalPutValue[i] = Value$DigitalPutValue
}
BINInputData$StockPrice = inputStockPrice # Reset from previous analysis
# Plots: Plain Vanilla Options with respect to the Stock Price
MaxXValue = max(StockPrice); MinXValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
#  Call with boundaries
MaxYValue = max(ESPlainVanillaCallValue, ASPlainVanillaCallValue,
  ESPlainVanillaCallLB, ESPlainVanillaCallUB,
  ASPlainVanillaCallLB, ASPlainVanillaCallUB)
MinYValue = min(ESPlainVanillaCallValue, ASPlainVanillaCallValue, 
  ESPlainVanillaCallLB, ESPlainVanillaCallUB,
  ASPlainVanillaCallLB, ASPlainVanillaCallUB)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Call Value"
legtxt = c("ES Call Value", "AS Call Value", 
  "ES Call Lower Bound", "ES Call Upper Bound",
  "AS Call Lower Bound", "AS Call Upper Bound")
lTitle = "Parameter"
plot(StockPrice, ESPlainVanillaCallValue, type = "l", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPlainVanillaCallValue, type = "l", col ="red", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESPlainVanillaCallLB, type = "l", col ="green", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ESPlainVanillaCallUB, type = "l", col ="purple", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
lines(StockPrice, ASPlainVanillaCallLB, type = "l", col ="blue4", 
  xlim = xlim1, ylim = ylim1, pch = 5, cex = 0.5)
lines(StockPrice, ASPlainVanillaCallUB, type = "l", col ="brown", 
  xlim = xlim1, ylim = ylim1, pch = 6, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1,1,1), 
  lty = c(1,1,1,1,1,1),
  col = c("blue", "red", "green", "purple", "blue4", "brown"), 
  pch = c(NA,NA,NA,NA,NA,NA), bty = "n", title = lTitle)
#  Call time value
ESPVCallTV <- ESPlainVanillaCallValue - ESPlainVanillaCallLB
ASPVCallTV <- ASPlainVanillaCallValue - ASPlainVanillaCallLB
MaxValue = max(ESPVCallTV, ASPVCallTV)
MinValue = min(ESPVCallTV, ASPVCallTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "ES and AS Binomial Option Time Value (ABM)"
xTitle = "Stock Price"
yTitle = "Call Time Value"
legtxt = c("ES Call Time Value", "AS Call Time Value")
lTitle = "Parameter"
plot(StockPrice, ESPVCallTV, type = "l", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPVCallTV, type = "l", col ="red", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(1,1),
  col = c("blue", "red"), pch = c(NA,NA), bty = "n", title = lTitle)
# Put with boundaries
MaxYValue = max(ESPlainVanillaPutValue, ASPlainVanillaPutValue, 
  ESPlainVanillaPutLB, ESPlainVanillaPutUB,
  ASPlainVanillaPutLB, ASPlainVanillaPutUB)
MinYValue = min(ESPlainVanillaPutValue, ASPlainVanillaPutValue,  
  ESPlainVanillaPutLB, ASPlainVanillaPutUB)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
legtxt = c("ES Put Value", "AS Put Value", 
  "ES Put Lower Bound", "ES Put Upper Bound",
  "AS Put Lower Bound", "AS Put Upper Bound")
mTitle = "Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Put Value"
lTitle = "Parameter"
plot(StockPrice, ESPlainVanillaPutValue, type = "l", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPlainVanillaPutValue, type = "l", col ="red", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESPlainVanillaPutLB, type = "l", col ="green", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ESPlainVanillaPutUB, type = "l", col ="purple", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
lines(StockPrice, ASPlainVanillaPutLB, type = "l", col ="blue4", 
  xlim = xlim1, ylim = ylim1, pch = 5, cex = 0.5)
lines(StockPrice, ASPlainVanillaPutUB, type = "l", col ="brown", 
  xlim = xlim1, ylim = ylim1, pch = 6, cex = 0.5)
legend("right", legtxt, cex = 0.75, lwd = c(1,1,1,1,1,1), 
  lty = c(1,1,1,1,1,1),
  col = c("blue", "red", "green", "purple", "blue4", "brown"), 
  pch = c(NA,NA,NA,NA,NA,NA), bty = "n", title = lTitle)
#  Put time value
ESPVPutTV <- ESPlainVanillaPutValue - ESPlainVanillaPutLB
ASPVPutTV <- ASPlainVanillaPutValue - ASPlainVanillaPutLB
MaxValue = max(ESPVPutTV, ASPVPutTV)
MinValue = min(ESPVPutTV, ASPVPutTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "ES and AS Binomial Option Time Value (ABM)"
xTitle = "Stock Price"
yTitle = "Put Time Value"
legtxt = c("ES Put Time Value", "AS Put Time Value")
lTitle = "Parameter"
plot(StockPrice, ESPVPutTV, type = "l", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPVPutTV, type = "l", col ="red", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(1,1),
  col = c("blue", "red"), pch = c(NA,NA), bty = "n", title = lTitle)
# Calls and puts
MaxYValue = max(ESPlainVanillaCallValue, ASPlainVanillaCallValue,
  ESPlainVanillaPutValue, ASPlainVanillaPutValue)
MinYValue = min(ESPlainVanillaCallValue, ASPlainVanillaCallValue,
  ESPlainVanillaPutValue, ASPlainVanillaPutValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(StockPrice); MinXValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
legtxt = c("ES Call Value", "AS Call Value", "ES Put Value", "AS Put Value")
mTitle = "Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Option Value"
lTitle = "Parameter"
plot(StockPrice, ESPlainVanillaCallValue, type = "l", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASPlainVanillaCallValue, type = "l", col ="red", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESPlainVanillaPutValue, type = "l", col ="green", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ASPlainVanillaPutValue, type = "l", col ="purple", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1),
  col = c("blue","red","green","purple"), pch = c(NA,NA,NA,NA), bty = "n", 
  title = lTitle)
# Digital Options with respect to the Stock Price
MaxYValue = max(ESDigitalCallValue, ASDigitalCallValue,
  ESDigitalPutValue, ASDigitalPutValue)
MinYValue = min(ESDigitalCallValue, ASDigitalCallValue,
  ESDigitalPutValue, ASDigitalPutValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(StockPrice); MinXValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
legtxt = c("ES Call Value", "AS Call Value", "ES Put Value", "AS Put Value")
mTitle = "Binomial Digital Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Digital Option Value"
lTitle = "Parameter"
plot(StockPrice, ESDigitalCallValue, type = "l", main = mTitle,
  sub = sTitleBINDP, xlab = xTitle, ylab = yTitle, col = "blue", 
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ASDigitalCallValue, type = "l", col ="red", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESDigitalPutValue, type = "l", col ="green", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, ASDigitalPutValue, type = "l", col ="purple", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("bottom", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1),
  col = c("blue","red","green","purple"), pch = c(NA,NA,NA,NA), bty = "n", 
  title = lTitle)
#
# Plots with Number Of Steps
#
StepCount = as.integer((as.numeric(MaxStep)-as.numeric(MinStep)) /
  as.numeric(StepSize) + 1)
NumberOfSteps = as.integer(seq(MinStep,MaxStep,StepSize))
ESPlainVanillaCallValue <- numeric(StepCount)
ESPlainVanillaPutValue <- numeric(StepCount)
ESDigitalCallValue <- numeric(StepCount)
ESDigitalPutValue <- numeric(StepCount)
ASPlainVanillaCallValue <- numeric(StepCount)
ASPlainVanillaPutValue <- numeric(StepCount)
ASDigitalCallValue <- numeric(StepCount)
ASDigitalPutValue <- numeric(StepCount)
j=0L
for(i in seq(MinStep, MaxStep, StepSize)){
  j = j + 1
  BINInputData$NumberOfSteps = as.integer(i)
  Value <- ABMESOptionValue(BINInputData)
  ESPlainVanillaCallValue[j] = Value$CallValue
  ESPlainVanillaPutValue[j] = Value$PutValue
  ESDigitalCallValue[j] = Value$DigitalCallValue
  ESDigitalPutValue[j] = Value$DigitalPutValue
  Value <- ABMASOptionValue(BINInputData)
  ASPlainVanillaCallValue[j] = Value$CallValue
  ASPlainVanillaPutValue[j] = Value$PutValue
  ASDigitalCallValue[j] = Value$DigitalCallValue
  ASDigitalPutValue[j] = Value$DigitalPutValue
}
BINInputData$NumberOfSteps = inputNumberOfSteps
# Plain vanilla option values with respect to the number of steps
xlim1 = c(1:2); xlim1[1] = MinStep; xlim1[2] = MaxStep
MaxOptionValue = max(ESPlainVanillaCallValue, ASPlainVanillaCallValue,
  ESPlainVanillaPutValue, ASPlainVanillaPutValue)
MinOptionValue = min(ESPlainVanillaCallValue, ASPlainVanillaCallValue,
  ESPlainVanillaPutValue, ASPlainVanillaPutValue)
ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
legtxt = c("ES Call Value", "AS Call Value", "ES Put Value", "AS Put Value")
mTitle = "Binomial Option Value (ABM)"
xTitle = "Number of Steps"
yTitle = "Option Value"
lTitle = "Parameter"
plot(NumberOfSteps, ESPlainVanillaCallValue, type = "l", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue",
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(NumberOfSteps, ASPlainVanillaCallValue, type = "l", col ="red",
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(NumberOfSteps, ESPlainVanillaPutValue, type = "l", col ="green",
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(NumberOfSteps, ASPlainVanillaPutValue, type = "l", col ="purple",
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1),
  col = c("blue","red","green","purple"), pch = c(NA,NA,NA,NA), bty = "n",
  title = lTitle)
# Digital Options with respect to the Stock Price
MaxYValue = max(ESDigitalCallValue, ASDigitalCallValue,
  ESDigitalPutValue, ASDigitalPutValue)
MinYValue = min(ESDigitalCallValue, ASDigitalCallValue,
  ESDigitalPutValue, ASDigitalPutValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
xlim1 = c(1:2); xlim1[1] = MinStep; xlim1[2] = MaxStep
legtxt = c("ES Call Value", "AS Call Value", "ES Put Value", "AS Put Value")
mTitle = "Binomial Digital Option Value (ABM)"
xTitle = "Number of Steps"
yTitle = "Digital Option Value"
lTitle = "Parameter"
plot(NumberOfSteps, ESDigitalCallValue, type = "l", main = mTitle,
  sub = sTitleBINDP, xlab = xTitle, ylab = yTitle, col = "blue",
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(NumberOfSteps, ASDigitalCallValue, type = "l", col ="red",
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(NumberOfSteps, ESDigitalPutValue, type = "l", col ="green",
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(NumberOfSteps, ASDigitalPutValue, type = "l", col ="purple",
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("bottom", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(1,1,1,1),
  col = c("blue","red","green","purple"), pch = c(NA,NA,NA,NA), bty = "n",
  title = lTitle)

beep(sound = 10, print('Finished')) # Helpful when running long program
