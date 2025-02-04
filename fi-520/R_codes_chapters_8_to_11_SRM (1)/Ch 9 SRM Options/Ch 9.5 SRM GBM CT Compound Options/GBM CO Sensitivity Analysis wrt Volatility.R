# GBM Sensitivity Analysis wrt Volatility.R
# # Plots with Volatility
StepSize = (VUpperBound - VLowerBound)/(NumberOfObservations - 1)
Volatility <- c(1:NumberOfObservations)
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
  Volatility[i] <-VLowerBound + (i - 1)*StepSize
  # Volatility[i] <- inputVolatility
  COInputData$v <- Volatility[i]
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
COInputData$v = inputVolatility
COInputData$iC <- 1
COInputData$iU <- 1
# Footer: Generic
TS = paste0(', S=', COInputData$S)
TXU = paste0(', XU=', COInputData$XU)
TR = paste0(', r=', round(COInputData$r,4))
TQ = paste0(', q=', round(COInputData$q,4))
TD = paste0(', d=', round(COInputData$d,4))
TTU = paste0(', TU=', COInputData$TU)
TTC = paste0(', TC=', COInputData$TC)
# TSig = paste0(', Vol=', COInputData$v)
lTitle = "Parameter"# x-axis
#
# CoC
#
TUO = paste0('UO=', round(UCallOption,2))
sTitle = paste0(TUO, TS, TXU, TR, TQ, TD, TTU, TTC)
MaxXValue = max(Volatility)
MinXValue = min(Volatility)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# Value Plot
MaxYValue = max(CoCValue)
MinYValue = min(CoCValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Option Values"
xTitle = "Volatility"
yTitle = "Values"
plot(Volatility, CoCValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Delta Plot
MaxYValue = max(CoCDelta)
MinYValue = min(CoCDelta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Delta"
xTitle = "Volatility"
yTitle = "Delta"
plot(Volatility, CoCDelta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(CoCGamma)
MinYValue = min(CoCGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Gamma"
xTitle = "Volatility"
yTitle = "Gamma"
plot(Volatility, CoCGamma, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(CoCTheta)
MinYValue = min(CoCTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Theta"
xTitle = "Volatility"
yTitle = "Theta"
plot(Volatility, CoCTheta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(CoCVega)
MinYValue = min(CoCVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Vega"
xTitle = "Volatility"
yTitle = "Vega"
plot(Volatility, CoCVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(CoCRho)
MinYValue = min(CoCRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Rho"
xTitle = "Volatility"
yTitle = "Rho"
plot(Volatility, CoCRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

#
# CoP
#
TUO = paste0('UO=', round(UPutOption,2))
sTitle = paste0(TUO, TS, TXU, TR, TQ, TD, TTU, TTC)
# Set x range
MaxXValue = max(Volatility)
MinXValue = min(Volatility)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# Value Plot
MaxYValue = max(CoPValue)
MinYValue = min(CoPValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Option Values"
xTitle = "Volatility"
yTitle = "Values"
plot(Volatility, CoPValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Delta Plot
MaxYValue = max(CoPDelta)
MinYValue = min(CoPDelta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Delta"
xTitle = "Volatility"
yTitle = "Delta"
plot(Volatility, CoPDelta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(CoPGamma)
MinYValue = min(CoPGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Gamma"
xTitle = "Volatility"
yTitle = "Gamma"
plot(Volatility, CoPGamma, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(CoPTheta)
MinYValue = min(CoPTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Theta"
xTitle = "Volatility"
yTitle = "Theta"
plot(Volatility, CoPTheta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(CoPVega)
MinYValue = min(CoPVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Vega"
xTitle = "Volatility"
yTitle = "Vega"
plot(Volatility, CoPVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(CoPRho)
MinYValue = min(CoPRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Rho"
xTitle = "Volatility"
yTitle = "Rho"
plot(Volatility, CoPRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

#
# PoC
#
TUO = paste0('UO=', round(UCallOption,2))
sTitle = paste0(TUO, TS, TXU, TR, TQ, TD, TTU, TTC)
# Set x range
MaxXValue = max(Volatility)
MinXValue = min(Volatility)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# Value Plot
MaxYValue = max(PoCValue)
MinYValue = min(PoCValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Option Values"
xTitle = "Volatility"
yTitle = "Values"
plot(Volatility, PoCValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Delta Plot
MaxYValue = max(PoCDelta)
MinYValue = min(PoCDelta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Delta"
xTitle = "Volatility"
yTitle = "Delta"
plot(Volatility, PoCDelta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(PoCGamma)
MinYValue = min(PoCGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Gamma"
xTitle = "Volatility"
yTitle = "Gamma"
plot(Volatility, PoCGamma, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(PoCTheta)
MinYValue = min(PoCTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Theta"
xTitle = "Volatility"
yTitle = "Theta"
plot(Volatility, PoCTheta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(PoCVega)
MinYValue = min(PoCVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Vega"
xTitle = "Volatility"
yTitle = "Vega"
plot(Volatility, PoCVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(PoCRho)
MinYValue = min(PoCRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Rho"
xTitle = "Volatility"
yTitle = "Rho"
plot(Volatility, PoCRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

#
# PoP
#
TUO = paste0('UO=', round(UPutOption,2))
sTitle = paste0(TUO, TS, TXU, TR, TQ, TD, TTU, TTC)
# Set x range
MaxXValue = max(Volatility)
MinXValue = min(Volatility)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# Value Plot
MaxYValue = max(PoPValue)
MinYValue = min(PoPValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Option Values"
xTitle = "Volatility"
yTitle = "Values"
plot(Volatility, PoPValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Delta Plot
MaxYValue = max(PoPDelta)
MinYValue = min(PoPDelta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Delta"
xTitle = "Volatility"
yTitle = "Delta"
plot(Volatility, PoPDelta, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(PoPGamma)
MinYValue = min(PoPGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Gamma"
xTitle = "Volatility"
yTitle = "Gamma"
plot(Volatility, PoPGamma, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(PoPTheta)
MinYValue = min(PoPTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Theta"
xTitle = "Volatility"
yTitle = "Theta"
plot(Volatility, PoPTheta, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(PoPVega)
MinYValue = min(PoPVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Vega"
xTitle = "Volatility"
yTitle = "Vega"
plot(Volatility, PoPVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(PoPRho)
MinYValue = min(PoPRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Rho"
xTitle = "Volatility"
yTitle = "Rho"
plot(Volatility, PoPRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)


