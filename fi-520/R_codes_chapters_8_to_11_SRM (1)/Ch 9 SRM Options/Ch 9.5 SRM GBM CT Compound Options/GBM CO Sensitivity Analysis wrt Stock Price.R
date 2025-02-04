# GBM Sensitivity Analysis wrt Stock Price.R
# # Plots with StockPrice
StepSize = (SUpperBound - SLowerBound)/(NumberOfObservations - 1)
StockPrice <- c(1:NumberOfObservations)
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
  StockPrice[i] <-SLowerBound + (i - 1)*StepSize
  COInputData$S <- StockPrice[i]
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
COInputData$S = inputUnderlying
COInputData$iC <- 1
COInputData$iU <- 1
# Footer: Generic
TXU = paste0(' XU=', COInputData$XU)
TR = paste0(', r=', round(COInputData$r,4))
TQ = paste0(', q=', round(COInputData$q,4))
TD = paste0(', d=', round(COInputData$d,4))
TTU = paste0(', TU=', COInputData$TU)
TTC = paste0(', TC=', COInputData$TC)
TSig = paste0(', Vol=', COInputData$v)
lTitle = "Parameter"# x-axis
#
# CoC
#
TUO = paste0('UO=', round(UCallOption,2))
sTitle = paste0(TUO, TXU, TR, TQ, TD, TTU, TTC, TSig)
# Set x range
z <- -99
for(i in 2:NumberOfObservations){
  if(CoCLowerBound[i] > CoCLowerBound[i-1] && z < 0){
    MidPoint <- StockPrice[i]
    z <- 99
  }  
}
# Range <- 0.5
# MaxXValue = MidPoint * (1 + Range)
# MinXValue = MidPoint * (1 - Range)
MinXValue = SLowerBound + 0.01
MaxXValue = SUpperBound - 0.01
xlim1 = c(1:2)
xlim1[1] = MinXValue
xlim1[2] = MaxXValue
Maxz <- -99
Minz <- -99
for(i in 1:NumberOfObservations){
  if(StockPrice[i] > MaxXValue && Maxz < 0){
    MaxXi <- i
    Maxz <- 99
  }  
  if(StockPrice[i] > MinXValue && Minz < 0){
    MinXi <- i
    Minz <- 99
  }  
}
# Value Plot
MaxYValue = max(CoCUpperBound[MaxXi])
MinYValue = min(CoCLowerBound[MinXi])
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
legtxt = c("CoC Value","CoC Lower Bound", "CoC Upper Bound")
mTitle = "Call on Call Option Values and Boundaries"
xTitle = "Stock Price"
yTitle = "Values"
plot(StockPrice, CoCValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, CoCLowerBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CoCUpperBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0),
  col = c("black","black","black"), pch = c(1,2,3), bty = "n",
  title = lTitle)
# Delta Plot
MaxYValue = max(CoCDelta[MaxXi])
MinYValue = min(CoCDelta[MinXi])
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Delta"
xTitle = "Stock Price"
yTitle = "Delta"
plot(StockPrice, CoCDelta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(CoCGamma)
MinYValue = min(CoCGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Gamma"
xTitle = "Stock Price"
yTitle = "Gamma"
plot(StockPrice, CoCGamma, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(CoCTheta)
MinYValue = min(CoCTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Theta"
xTitle = "Stock Price"
yTitle = "Theta"
plot(StockPrice, CoCTheta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(CoCVega)
MinYValue = min(CoCVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Vega"
xTitle = "Stock Price"
yTitle = "Vega"
plot(StockPrice, CoCVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(CoCRho)
MinYValue = min(CoCRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Call Rho"
xTitle = "Stock Price"
yTitle = "Rho"
plot(StockPrice, CoCRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

#
# CoP
#
TUO = paste0('UO=', round(UPutOption,2))
sTitle = paste0(TUO, TXU, TR, TQ, TD, TTU, TTC, TSig)
# Set x range
z <- -99
for(i in 2:NumberOfObservations){
  if(CoPLowerBound[i] >= CoPLowerBound[i-1] && z < 0){
    MidPoint <- StockPrice[i-2]
    z <- 99
  }  
}
# Range <- 0.5
# MaxXValue = MidPoint * (1 + Range)
# MinXValue = MidPoint * (1 - Range)
MinXValue = SLowerBound + 0.01
MaxXValue = SUpperBound - 0.01
xlim1 = c(1:2)
xlim1[1] = MinXValue
xlim1[2] = MaxXValue
Maxz <- -99
Minz <- -99
for(i in 1:NumberOfObservations){
  if(StockPrice[i] > MaxXValue && Maxz < 0){
    MaxXi <- i
    Maxz <- 99
  }  
  if(StockPrice[i] > MinXValue && Minz < 0){
    MinXi <- i
    Minz <- 99
  }  
}
# Value Plot
MaxYValue = CoPUpperBound[MinXi]
MinYValue = CoPLowerBound[MaxXi]
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
legtxt = c("CoP Value","CoP Lower Bound", "CoP Upper Bound")
mTitle = "Call on Put Option Values and Boundaries"
xTitle = "Stock Price"
yTitle = "Values"
plot(StockPrice, CoPValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, CoPLowerBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CoPUpperBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0),
  col = c("black","black","black"), pch = c(1,2,3), bty = "n",
  title = lTitle)
# Delta Plot
MaxYValue = max(CoPDelta[MaxXi])
MinYValue = min(CoPDelta[MinXi])
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Delta"
xTitle = "Stock Price"
yTitle = "Delta"
plot(StockPrice, CoPDelta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(CoPGamma)
MinYValue = min(CoPGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Gamma"
xTitle = "Stock Price"
yTitle = "Gamma"
plot(StockPrice, CoPGamma, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(CoPTheta)
MinYValue = min(CoPTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Theta"
xTitle = "Stock Price"
yTitle = "Theta"
plot(StockPrice, CoPTheta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(CoPVega)
MinYValue = min(CoPVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Vega"
xTitle = "Stock Price"
yTitle = "Vega"
plot(StockPrice, CoPVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(CoPRho)
MinYValue = min(CoPRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Call on Put Rho"
xTitle = "Stock Price"
yTitle = "Rho"
plot(StockPrice, CoPRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

#
# PoC
#
TUO = paste0('UO=', round(UCallOption,2))
sTitle = paste0(TUO, TXU, TR, TQ, TD, TTU, TTC, TSig)
# Set x range
z <- -99
for(i in 2:NumberOfObservations){
  if(PoCLowerBound[i] >= PoCLowerBound[i-1] && z < 0){
    MidPoint <- StockPrice[i-1]
    z <- 99
  }  
}
# Range <- 0.5
# MaxXValue = MidPoint * (1 + Range)
# MinXValue = MidPoint * (1 - Range)
MinXValue = SLowerBound + 0.01
MaxXValue = SUpperBound - 0.01
xlim1 = c(1:2)
xlim1[1] = MinXValue
xlim1[2] = MaxXValue
Maxz <- -99
Minz <- -99
for(i in 1:NumberOfObservations){
  if(StockPrice[i] > MaxXValue && Maxz < 0){
    MaxXi <- i
    Maxz <- 99
  }  
  if(StockPrice[i] > MinXValue && Minz < 0){
    MinXi <- i
    Minz <- 99
  }  
}
# Value Plot
MaxYValue = max(PoCUpperBound[MinXi])
MinYValue = min(PoCLowerBound[MaxXi])
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
legtxt = c("PoC Value","PoC Lower Bound", "PoC Upper Bound")
mTitle = "Put on Call Option Values and Boundaries"
xTitle = "Stock Price"
yTitle = "Values"
plot(StockPrice, PoCValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PoCLowerBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, PoCUpperBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("bottomleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0),
  col = c("black","black","black"), pch = c(1,2,3), bty = "n",
  title = lTitle)
# Delta Plot
MaxYValue = max(PoCDelta)
MinYValue = min(PoCDelta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Delta"
xTitle = "Stock Price"
yTitle = "Delta"
plot(StockPrice, PoCDelta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(PoCGamma)
MinYValue = min(PoCGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Gamma"
xTitle = "Stock Price"
yTitle = "Gamma"
plot(StockPrice, PoCGamma, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(PoCTheta)
MinYValue = min(PoCTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Theta"
xTitle = "Stock Price"
yTitle = "Theta"
plot(StockPrice, PoCTheta, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(PoCVega)
MinYValue = min(PoCVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Vega"
xTitle = "Stock Price"
yTitle = "Vega"
plot(StockPrice, PoCVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(PoCRho)
MinYValue = min(PoCRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Call Rho"
xTitle = "Stock Price"
yTitle = "Rho"
plot(StockPrice, PoCRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

#
# PoP
#
TUO = paste0('UO=', round(UPutOption,2))
sTitle = paste0(TUO, TXU, TR, TQ, TD, TTU, TTC, TSig)
# Set x range
z <- -99
for(i in 2:NumberOfObservations){
  if(PoPLowerBound[i] > PoPLowerBound[i-1] && z < 0){
    MidPoint <- StockPrice[i-1]
    z <- 99
  }  
}
# Range <- 0.5
# MaxXValue = MidPoint * (1 + Range)
# MinXValue = MidPoint * (1 - Range)
MinXValue = SLowerBound + 0.01
MaxXValue = SUpperBound - 0.01
xlim1 = c(1:2)
xlim1[1] = MinXValue
xlim1[2] = MaxXValue
Maxz <- -99
Minz <- -99
for(i in 1:NumberOfObservations){
  if(StockPrice[i] > MaxXValue && Maxz < 0){
    MaxXi <- i
    Maxz <- 99
  }  
  if(StockPrice[i] > MinXValue && Minz < 0){
    MinXi <- i
    Minz <- 99
  }  
}
# Value Plot
MaxYValue = PoPUpperBound[MaxXi]
MinYValue = PoPLowerBound[MinXi]
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
legtxt = c("PoP Value","PoP Lower Bound", "PoP Upper Bound")
mTitle = "Put on Put Option Values and Boundaries"
xTitle = "Stock Price"
yTitle = "Values"
plot(StockPrice, PoPValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PoPLowerBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, PoPUpperBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0),
  col = c("black","black","black"), pch = c(1,2,3), bty = "n",
  title = lTitle)
# Delta Plot
MaxYValue = max(PoPDelta)
MinYValue = min(PoPDelta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Delta"
xTitle = "Stock Price"
yTitle = "Delta"
plot(StockPrice, PoPDelta, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Gamma Plot
MaxYValue = max(PoPGamma)
MinYValue = min(PoPGamma)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Gamma"
xTitle = "Stock Price"
yTitle = "Gamma"
plot(StockPrice, PoPGamma, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Theta Plot
MaxYValue = max(PoPTheta)
MinYValue = min(PoPTheta)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Theta"
xTitle = "Stock Price"
yTitle = "Theta"
plot(StockPrice, PoPTheta, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)

# Vega Plot
MaxYValue = max(PoPVega)
MinYValue = min(PoPVega)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Vega"
xTitle = "Stock Price"
yTitle = "Vega"
plot(StockPrice, PoPVega, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
# Rho Plot
MaxYValue = max(PoPRho)
MinYValue = min(PoPRho)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle = "Put on Put Rho"
xTitle = "Stock Price"
yTitle = "Rho"
plot(StockPrice, PoPRho, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)


