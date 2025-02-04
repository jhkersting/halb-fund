# GBM Sensitivity Analysis wrt Option Yield.R
# # Plots with OptionYield
StepSize = (qUpperBound - qLowerBound)/(NumberOfObservations - 1)
OptionYield <- c(1:NumberOfObservations)
CoCLowerBound <- c(1:NumberOfObservations)
CoPLowerBound <- c(1:NumberOfObservations)
PoCLowerBound <- c(1:NumberOfObservations)
PoPLowerBound <- c(1:NumberOfObservations)
CoCUpperBound <- c(1:NumberOfObservations)
CoPUpperBound <- c(1:NumberOfObservations)
PoCUpperBound <- c(1:NumberOfObservations)
PoPUpperBound <- c(1:NumberOfObservations)
CoCValue <- c(1:NumberOfObservations)
CoPValue <- c(1:NumberOfObservations)
PoCValue <- c(1:NumberOfObservations)
PoPValue <- c(1:NumberOfObservations)
CoCDelta <- c(1:NumberOfObservations)
CoPDelta <- c(1:NumberOfObservations)
PoCDelta <- c(1:NumberOfObservations)
PoPDelta <- c(1:NumberOfObservations)
CoCGamma <- c(1:NumberOfObservations)
CoPGamma <- c(1:NumberOfObservations)
PoCGamma <- c(1:NumberOfObservations)
PoPGamma <- c(1:NumberOfObservations)
CoCTheta <- c(1:NumberOfObservations)
CoPTheta <- c(1:NumberOfObservations)
PoCTheta <- c(1:NumberOfObservations)
PoPTheta <- c(1:NumberOfObservations)

CoCVega <- c(1:NumberOfObservations)
CoPVega <- c(1:NumberOfObservations)
PoCVega <- c(1:NumberOfObservations)
PoPVega <- c(1:NumberOfObservations)
CoCRho <- c(1:NumberOfObservations)
CoPRho <- c(1:NumberOfObservations)
PoCRho <- c(1:NumberOfObservations)
PoPRho <- c(1:NumberOfObservations)

for(i in 1:NumberOfObservations){
  OptionYield[i] <- qLowerBound + (i - 1)*StepSize
  # OptionYield[i] <- inputOptionYield
  COInputData$q <- OptionYield[i]
  COInputData$iC <- 1
  COInputData$iU <- 1
  COInputData$XC <- UCallOption
  CoCLowerBound[i] <- COLowerBound(COInputData)
  CoCUpperBound[i] <- COUpperBound(COInputData)
  CoCValue[i] <- COValue(COInputData, LowerBound, UpperBound) 
  CoCDelta[i] <- CODelta(COInputData, LowerBound, UpperBound) 
  CoCGamma[i] <- COGamma(COInputData, LowerBound, UpperBound) 
  CoCTheta[i] <- COTheta(COInputData, LowerBound, UpperBound) 
  
  CoCVega[i] <- COVega(COInputData, LowerBound, UpperBound) 
  CoCRho[i] <- CORho(COInputData, LowerBound, UpperBound) 
  
  COInputData$iC <- 1
  COInputData$iU <- -1
  COInputData$XC <- UPutOption
  CoPLowerBound[i] <- COLowerBound(COInputData)
  CoPUpperBound[i] <- COUpperBound(COInputData)
  CoPValue[i] <- COValue(COInputData, LowerBound, UpperBound) 
  CoPDelta[i] <- CODelta(COInputData, LowerBound, UpperBound) 
  CoPGamma[i] <- COGamma(COInputData, LowerBound, UpperBound) 
  CoPTheta[i] <- COTheta(COInputData, LowerBound, UpperBound) 
  
  CoPVega[i] <- COVega(COInputData, LowerBound, UpperBound) 
  CoPRho[i] <- CORho(COInputData, LowerBound, UpperBound) 
  
  COInputData$iC <- -1
  COInputData$iU <- 1
  COInputData$XC <- UCallOption
  PoCLowerBound[i] <- COLowerBound(COInputData)
  PoCUpperBound[i] <- COUpperBound(COInputData)
  PoCValue[i] <- COValue(COInputData, LowerBound, UpperBound) 
  PoCDelta[i] <- CODelta(COInputData, LowerBound, UpperBound) 
  PoCGamma[i] <- COGamma(COInputData, LowerBound, UpperBound) 
  PoCTheta[i] <- COTheta(COInputData, LowerBound, UpperBound) 
  
  PoCVega[i] <- COVega(COInputData, LowerBound, UpperBound) 
  PoCRho[i] <- CORho(COInputData, LowerBound, UpperBound) 
  
  COInputData$iC <- -1
  COInputData$iU <- -1
  COInputData$XC <- UPutOption
  PoPLowerBound[i] <- COLowerBound(COInputData)
  PoPUpperBound[i] <- COUpperBound(COInputData)
  PoPValue[i] <- COValue(COInputData, LowerBound, UpperBound) 
  PoPDelta[i] <- CODelta(COInputData, LowerBound, UpperBound) 
  PoPGamma[i] <- COGamma(COInputData, LowerBound, UpperBound) 
  PoPTheta[i] <- COTheta(COInputData, LowerBound, UpperBound) 
  
  PoPVega[i] <- COVega(COInputData, LowerBound, UpperBound) 
  PoPRho[i] <- CORho(COInputData, LowerBound, UpperBound) 
  
}
# Reset input values
COInputData$q = inputOptionYield
COInputData$iC <- 1
COInputData$iU <- 1
# Footer: Generic
TS = paste0(', S=', COInputData$S)
TXU = paste0(', XU=', COInputData$XU)
TR = paste0(', r=', round(COInputData$r,4))
# TQ = paste0(', q=', round(COInputData$q,4))
TD = paste0(', d=', round(COInputData$d,4))
TTU = paste0(', TU=', COInputData$TU)
TTC = paste0(', TC=', COInputData$TC)
TSig = paste0(', Vol=', COInputData$v)
lTitle = "Parameter"# x-axis
#
# CoC
#
TUO = paste0('UO=', round(UCallOption,2))
sTitle = paste0(TUO, TS, TXU, TR, TD, TTU, TTC, TSig)
MaxXValue = max(OptionYield)
MinXValue = min(OptionYield)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# Value Plot
MaxYValue = max(CoCValue)
MinYValue = min(CoCValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Option Values"
xTitle = "Option Yield"
yTitle = "Values"
plot(OptionYield, CoCValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Delta Plot
MaxYValue = max(CoCDelta)
MinYValue = min(CoCDelta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Delta"
xTitle = "Option Yield"
yTitle = "Delta"
plot(OptionYield, CoCDelta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(CoCGamma)
MinYValue = min(CoCGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Gamma"
xTitle = "Option Yield"
yTitle = "Gamma"
plot(OptionYield, CoCGamma, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(CoCTheta)
MinYValue = min(CoCTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Theta"
xTitle = "Option Yield"
yTitle = "Theta"
plot(OptionYield, CoCTheta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(CoCVega)
MinYValue = min(CoCVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Vega"
xTitle = "Option Yield"
yTitle = "Vega"
plot(OptionYield, CoCVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(CoCRho)
MinYValue = min(CoCRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Rho"
xTitle = "Option Yield"
yTitle = "Rho"
plot(OptionYield, CoCRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

#
# CoP
#
TUO = paste0('UO=', round(UPutOption,2))
sTitle = paste0(TUO, TS, TXU, TR, TD, TTU, TTC, TSig)
# Set x range
MaxXValue = max(OptionYield)
MinXValue = min(OptionYield)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# Value Plot
MaxYValue = max(CoPValue)
MinYValue = min(CoPValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Option Values"
xTitle = "Option Yield"
yTitle = "Values"
plot(OptionYield, CoPValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Delta Plot
MaxYValue = max(CoPDelta)
MinYValue = min(CoPDelta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Delta"
xTitle = "Option Yield"
yTitle = "Delta"
plot(OptionYield, CoPDelta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(CoPGamma)
MinYValue = min(CoPGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Gamma"
xTitle = "Option Yield"
yTitle = "Gamma"
plot(OptionYield, CoPGamma, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(CoPTheta)
MinYValue = min(CoPTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Theta"
xTitle = "Option Yield"
yTitle = "Theta"
plot(OptionYield, CoPTheta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(CoPVega)
MinYValue = min(CoPVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Vega"
xTitle = "Option Yield"
yTitle = "Vega"
plot(OptionYield, CoPVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(CoPRho)
MinYValue = min(CoPRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Rho"
xTitle = "Option Yield"
yTitle = "Rho"
plot(OptionYield, CoPRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

#
# PoC
#
TUO = paste0('UO=', round(UCallOption,2))
sTitle = paste0(TUO, TS, TXU, TR, TD, TTU, TTC, TSig)
# Set x range
MaxXValue = max(OptionYield)
MinXValue = min(OptionYield)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# Value Plot
MaxYValue = max(PoCValue)
MinYValue = min(PoCValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Option Values"
xTitle = "Option Yield"
yTitle = "Values"
plot(OptionYield, PoCValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Delta Plot
MaxYValue = max(PoCDelta)
MinYValue = min(PoCDelta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Delta"
xTitle = "Option Yield"
yTitle = "Delta"
plot(OptionYield, PoCDelta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(PoCGamma)
MinYValue = min(PoCGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Gamma"
xTitle = "Option Yield"
yTitle = "Gamma"
plot(OptionYield, PoCGamma, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(PoCTheta)
MinYValue = min(PoCTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Theta"
xTitle = "Option Yield"
yTitle = "Theta"
plot(OptionYield, PoCTheta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(PoCVega)
MinYValue = min(PoCVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Vega"
xTitle = "Option Yield"
yTitle = "Vega"
plot(OptionYield, PoCVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(PoCRho)
MinYValue = min(PoCRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Rho"
xTitle = "Option Yield"
yTitle = "Rho"
plot(OptionYield, PoCRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

#
# PoP
#
TUO = paste0('UO=', round(UPutOption,2))
sTitle = paste0(TUO, TS, TXU, TR, TD, TTU, TTC, TSig)
# Set x range
MaxXValue = max(OptionYield)
MinXValue = min(OptionYield)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# Value Plot
MaxYValue = max(PoPValue)
MinYValue = min(PoPValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Option Values"
xTitle = "Option Yield"
yTitle = "Values"
plot(OptionYield, PoPValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Delta Plot
MaxYValue = max(PoPDelta)
MinYValue = min(PoPDelta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Delta"
xTitle = "Option Yield"
yTitle = "Delta"
plot(OptionYield, PoPDelta, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(PoPGamma)
MinYValue = min(PoPGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Gamma"
xTitle = "Option Yield"
yTitle = "Gamma"
plot(OptionYield, PoPGamma, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(PoPTheta)
MinYValue = min(PoPTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Theta"
xTitle = "Option Yield"
yTitle = "Theta"
plot(OptionYield, PoPTheta, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(PoPVega)
MinYValue = min(PoPVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Vega"
xTitle = "Option Yield"
yTitle = "Vega"
plot(OptionYield, PoPVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(PoPRho)
MinYValue = min(PoPRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Rho"
xTitle = "Option Yield"
yTitle = "Rho"
plot(OptionYield, PoPRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)


