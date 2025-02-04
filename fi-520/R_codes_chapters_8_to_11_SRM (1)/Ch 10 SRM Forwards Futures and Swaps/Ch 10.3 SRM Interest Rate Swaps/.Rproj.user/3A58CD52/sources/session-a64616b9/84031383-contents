# 10.3 IR Swap Valuation Test.R
# Model to value interest rate swaps based on two LSC curves
# rmarkdown::render("10.3 IR Swap Valuation Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
#  openxlsx - opening IRSwapBook.xlsx
#  tis: Time Indexes and Time Indexed Series (holidays)
Packages <- c("openxlsx", "date", "tis", "beepr") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# # Input dataset: IRSwapBook.xlsx # Swap book with detailed inputs
# FileName <- "IRSwapBook.xlsx"
# SVInputs <- read.xlsx(xlsxFile = FileName, sheet = "Book1", skipEmptyRows = TRUE)
# SVInputs
# Manual inputs
TradeName <- 'Trade1'
EM <- 1     # Evaluation month
ED <- 17    # Evaluation day
EY <- 2021  # Evaluation year
MM <- 1     # Maturity month
MD <- 17    # Maturity day
MY <- 2031  # Maturity year
FixPF <- 4  # Fixed payment frequency, times per year
FltPF <- 4  # Floating payment frequency, times per year
FixNAD <- 0  # Fixed number of accrued days: 0=30/FixNTD, 1=ACT/FixNTD
FltNAD <- 0  # Floating number of accrued days: 0=30/FixNTD, 1=ACT/FixNTD
FixNTD <- 360  # Day count convention for fixed leg
FltNTD <- 360  # Day count convention for floating leg
FixConv <- 'MBF' # Modified business following (MBF) or preceeding (MBP)
FltConv <- 'MBF'
SwapType <- 1 # 1 - receive fixed, -1 - receive floating
NAmt <- 1000.0 # Notional Amount
FixedRate <- 5.0 # Fixed swap rate
NLSCFR <- 3 # Number LSC forward rate factors
NLSCDR <- 3 # Number LSC discount rate factors
FRParm1 <- 5.0 # Forward rate LSC parameters
FRParm2 <- 0.0
FRParm3 <- 0.0
FRParm4 <- 0.0
FRParm5  <- 0.0
FRParm6  <- 0.0
DRParm1 <- 5.0 # Discount rate LSC parameters
DRParm2 <- 0.0
DRParm3 <- 0.0
DRParm4 <- 0.0
DRParm5 <- 0.0
DRParm6 <- 0.0
FRScalar1 <- 2.0 # Forward rate LSC scalars
FRScalar2 <- FRScalar1 # NOTE: Slope and Curve1 use different scalars perhaps
FRScalar3 <- 0.0
FRScalar4 <- 0.0
FRScalar5 <- 0.0
DRScalar1 <- 2.0 # Discount rate LSC scalars
DRScalar2 <- DRScalar1 # NOTE: Slope and Curve1 use different scalars perhaps
DRScalar3 <- 0.0
DRScalar4 <- 0.0
DRScalar5 <- 0.0
# Place all data in single data frame
SVInputs <- data.frame(TradeName, EM, ED, EY, MM, MD, MY, FixPF, FltPF, 
  FixNAD, FltNAD, FixNTD, FltNTD, FixConv, FltConv, SwapType, NAmt, FixedRate, 
  NLSCFR, NLSCDR, 
  FRParm1, FRParm2, FRParm3, FRParm4, FRParm5, FRParm6, 
  DRParm1, DRParm2, DRParm3, DRParm4, DRParm5, DRParm6, 
  FRScalar1, FRScalar2, FRScalar3, FRScalar4, FRScalar5, 
  DRScalar1, DRScalar2, DRScalar3, DRScalar4, DRScalar5)
names(SVInputs) <- c("TradeName", "EM", "ED", "EY", "MM", "MD", "MY", 
  "FixPF", "FltPF", "FixNAD", "FltNAD", "FixNTD", "FltNAD", "FixConv", 
  "FltConv", "SwapType", "NAmt", "FixedRate", "NLSCFR", "NLSCDR", 
  "FRParm1", "FRParm2", "FRParm3", "FRParm4", "FRParm5", "FRParm6", 
  "DRParm1", "DRParm2", "DRParm3", "DRParm4", "DRParm5", "DRParm6", 
  "FRScalar1", "FRScalar2", "FRScalar3", "FRScalar4", "FRScalar5", 
  "DRScalar1", "DRScalar2", "DRScalar3", "DRScalar4", "DRScalar5")
SVInputs
is.data.frame(SVInputs)
source('SWAP Functions.R')
# Swap tests
SWAP = SVInputs
# 'Rate1' uses two different curves, forward curve and discount curve (2 LSCs)
SWAP$Output = 'Rate1' 
TestSwapRate1 = SwapRate(SWAP)
TestSwapRate1
# 'Value1' uses two different curves, forward curve and discount curve (2 LSCs)
SWAP$Output = 'Value1' 
TestSwapValue1 = SwapRate(SWAP)
TestSwapValue1
SWAP = SVInputs
# 'Rate2' uses only discount curve (1 LSC)
SWAP$Output = 'Rate2' 
TestSwapRate2 = SwapRate(SWAP)
TestSwapRate2
SWAP = SVInputs
# 'Value2' uses only discount curve (1 LSC)
SWAP$Output = 'Value2' # Fit FC and DC separately with 2 LSCs
TestSwapValue2 = SwapRate(SWAP)
TestSwapValue2
# Plot footer information
TN = paste0(SWAP$TradeName)
TEval = paste0(', Eval. Date: ', SWAP$EM, '/', SWAP$ED, '/', SWAP$EY)
TMat = paste0(', Mat. Date: ', SWAP$MM, '/', SWAP$MD, '/', SWAP$MY)
TNAmt = paste0(', $', SWAP$NAmt)
TFR = paste0(', FR: L=', FRParm1, ', S=', FRParm2, ', C=', FRParm3)
TDR = paste0(', DR: L=', DRParm1, ', S=', DRParm2, ', C=', DRParm3)
sTitle = paste0(TN, TEval, TMat, TNAmt, TFR, TDR)
#
# Sensitivity Testing
#
# Swap value as function of fixed rate
#
OriginalFixRate = SWAP$FixedRate
N = 100
INCR = (OriginalFixRate - OriginalFixRate/2)/(N/2)
NP1 = N + 1
SV = as.numeric(c(1:NP1))
SR = as.numeric(c(1:NP1))
for(i in 1:NP1){
  SWAP$Output = 'Value1'
  SWAP$FixedRate = SR[i] = OriginalFixRate/2 + (i-1)*INCR
  SV[i] = SwapRate(SWAP)
  SWAP$Output = 'Rate1'
}
SWAP$FixedRate = OriginalFixRate  # Reset fixed rate to original value
# Plot swap value and swap rates
x <- SR
y <- SV
FixYRange = TRUE
if(FixYRange == TRUE){
  MaxYValue = 200
  MinYValue = -200
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Values")
} else {
  mTitle = paste0("Receive Floating Swap Values")
}
xTitle = "Swap Rates"
yTitle = "Swap Values"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue),
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Analysis of forward curve
#
# Swap value and rates as function of forward rate level
#
FRLevel = SWAP$FRParm1
N = 100
INCR = (FRLevel - FRLevel/2)/(N/2)
NP1 = N + 1
SV = as.numeric(c(1:NP1))
SR = as.numeric(c(1:NP1))
FRLevelV = as.numeric(c(1:NP1))
for(i in 1:NP1){
  SWAP$Output = 'Value1'
  SWAP$FRParm1 = FRLevelV[i] = FRLevel + (i-1-N/2)*INCR
  SV[i] = SwapRate(SWAP)
  SWAP$Output = 'Rate1'
  SR[i] = SwapRate(SWAP)
}
SWAP$FRParm1 = FRLevel
#
# Plot swap value and forward rate levels (same as fixed swap rate)
#
x <- FRLevelV
y <- SV
FixYRange = TRUE
if(FixYRange == TRUE){
  MaxYValue = 200
  MinYValue = -200
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Values")
} else {
  mTitle = paste0("Receive Floating Swap Values")
}
xTitle = "Forward Rate Levels"
yTitle = "Swap Values"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue),
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Plot swap rates and forward rate levels
#
x <- FRLevelV
y <- SR
FixYRange = FALSE
if(FixYRange == TRUE){
  MaxYValue = 100
  MinYValue = -100
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Rates")
} else {
  mTitle = paste0("Receive Floating Swap Rates")
}
xTitle = "Forward Rate Levels"
yTitle = "Swap Rates"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue),
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Swap value as function of forward rate slope
#
FRLevel = SWAP$FRParm1
FRSlope = SWAP$FRParm2
N = 100
INCR = (FRLevel - FRLevel/2)/(N/2) # Slopes often zero, use level
NP1 = N + 1
SV = as.numeric(c(1:NP1))
SR = as.numeric(c(1:NP1))
FRSlopeV = as.numeric(c(1:NP1))
for(i in 1:NP1){
  SWAP$Output = 'Value1'
  SWAP$FRParm2 = FRSlopeV[i] = FRSlope + (i-1-N/2)*INCR
  SV[i] = SwapRate(SWAP)
  SWAP$Output = 'Rate1'
  SR[i] = SwapRate(SWAP)
}
SWAP$FRParm2 = FRSlope
#
# Plot swap value and forward rate slopes
#
x <- FRSlopeV
y <- SV
FixYRange = TRUE
if(FixYRange == TRUE){
  MaxYValue = 100
  MinYValue = -100
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Values")
} else {
  mTitle = paste0("Receive Floating Swap Values")
}
xTitle = "Forward Rate Slope"
yTitle = "Swap Values"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue),
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
# lblX <- format.Date(lblX, "%b-%Y")
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Plot swap rates and forward rate slopes
#
x <- FRSlopeV
y <- SR
FixYRange = FALSE
if(FixYRange == TRUE){
  MaxYValue = 100
  MinYValue = -100
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Rates")
} else {
  mTitle = paste0("Receive Floating Swap Rates")
}
xTitle = "Forward Rate Slope"
yTitle = "Swap Rates"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue),
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Swap value as function of forward rate curvature 1
#
FRLevel = SWAP$FRParm1
FRCurvature1 = SWAP$FRParm3
N = 100
INCR = (FRLevel - FRLevel/2)/(N/2)
NP1 = N + 1
SV = as.numeric(c(1:NP1))
SR = as.numeric(c(1:NP1))
FRCurvature1V = as.numeric(c(1:NP1))
for(i in 1:NP1){
  SWAP$Output = 'Value1'
  SWAP$FRParm3 = FRCurvature1V[i] = FRCurvature1 + (i-1-N/2)*INCR
  SV[i] = SwapRate(SWAP)
  SWAP$Output = 'Rate1'
  SR[i] = SwapRate(SWAP)
}
SWAP$FRParm3 = FRCurvature1
#
# Plot swap value and forward rate curvature 1
#
x <- FRCurvature1V
y <- SV
FixYRange = FALSE
if(FixYRange == TRUE){
  MaxYValue = 100
  MinYValue = -100
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Values")
} else {
  mTitle = paste0("Receive Floating Swap Values")
}
xTitle = "Forward Rate Curvature 1"
yTitle = "Swap Values"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue),
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
# lblX <- format.Date(lblX, "%b-%Y")
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Plot swap rates and forward rate curvature 1
#
x <- FRCurvature1V
y <- SR
FixYRange = FALSE
if(FixYRange == TRUE){
  MaxYValue = 100
  MinYValue = -100
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Rates")
} else {
  mTitle = paste0("Receive Floating Swap Rates")
}
xTitle = "Forward Rate Curvature 1"
yTitle = "Swap Rates"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue),
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Analysis of discount curve
#
# Base Case
SWAP$Output = 'Rate1'
TestSwapRate = SwapRate(SWAP)
TestSwapRate
SWAP$Output = 'Value1'
TestSwapValue = SwapRate(SWAP)
TestSwapValue

#
# Swap value as function of discount rate level
# NOTE: If initial swap value is zero and flat curve, 
#  then discount rates do not matter (w/ two curves)
# 
DRLevel = SWAP$DRParm1
N = 100
INCR = (DRLevel - DRLevel/2)/(N/2)
NP1 = N + 1
SV = as.numeric(c(1:NP1))
SR = as.numeric(c(1:NP1))
DRLevelV = as.numeric(c(1:NP1))
for(i in 1:NP1){
  SWAP$Output = 'Value2'
  SWAP$DRParm1 = DRLevelV[i] = DRLevel + (i-1-N/2)*INCR
  SV[i] = SwapRate(SWAP)
  SWAP$Output = 'Rate2'
  SR[i] = SwapRate(SWAP)
}
SWAP$DRParm1 = DRLevel
#
# Plot swap value and discount rate levels
#
x <- DRLevelV
y <- SV
FixYRange = FALSE
if(FixYRange == TRUE){
  MaxYValue = 100
  MinYValue = -100
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Values")
} else {
  mTitle = paste0("Receive Floating Swap Values")
}
xTitle = "Discount Rate Levels"
yTitle = "Swap Values"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue), 
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Plot swap rates and discount rate levels
#
x <- DRLevelV
y <- SR
FixYRange = FALSE
if(FixYRange == TRUE){
  MaxYValue = 100
  MinYValue = -100
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Rates")
} else {
  mTitle = paste0("Receive Floating Swap Rates")
}
xTitle = "Discount Rate Levels"
yTitle = "Swap Rates"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue), 
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Swap value as function of discount rate slope
#
DRLevel = SWAP$DRParm1
DRSlope = SWAP$DRParm2
N = 100
INCR = (DRLevel - DRLevel/2)/(N/2)
NP1 = N + 1
SV = as.numeric(c(1:NP1))
SR = as.numeric(c(1:NP1))
DRSlopeV = as.numeric(c(1:NP1))
for(i in 1:NP1){
  SWAP$Output = 'Value2'
  SWAP$DRParm2 = DRSlopeV[i] = DRSlope + (i-1-N/2)*INCR
  SV[i] = SwapRate(SWAP)
  SWAP$Output = 'Rate2'
  SR[i] = SwapRate(SWAP)
}
SWAP$DRParm2 = DRSlope
#
# Plot swap value and discount rate slopes
#
x <- DRSlopeV
y <- SV
FixYRange = FALSE
if(FixYRange == TRUE){
  MaxYValue = 100
  MinYValue = -100
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Values")
} else {
  mTitle = paste0("Receive Floating Swap Values")
}
xTitle = "Discount Rate Slope"
yTitle = "Swap Values"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue),
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
# lblX <- format.Date(lblX, "%b-%Y")
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Plot swap rates and forward rate slopes
#
x <- DRSlopeV
y <- SR
FixYRange = FALSE
if(FixYRange == TRUE){
  MaxYValue = 100
  MinYValue = -100
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Rates")
} else {
  mTitle = paste0("Receive Floating Swap Rates")
}
xTitle = "Discount Rate Slope"
yTitle = "Swap Rates"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue),
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Swap value as function of discount rate curvature 1
#
DRLevel = SWAP$DRParm1
DRCurvature1 = SWAP$DRParm3
N = 100
INCR = (DRLevel - DRLevel/2)/(N/2)
NP1 = N + 1
SV = as.numeric(c(1:NP1))
SR = as.numeric(c(1:NP1))
DRCurvature1V = as.numeric(c(1:NP1))
# create a matrix to store the output with the discount rate
listsDR <- replicate(N, list(), simplify = FALSE)


for(i in 1:NP1){
  SWAP$Output = 'Value2'
  SWAP$DRParm3 = DRCurvature1V[i] = DRCurvature1 + (i-1-N/2)*INCR
  SV[i] = SwapRate(SWAP)
  SWAP$Output = 'Rate2'
  SR[i] = SwapRate(SWAP)
  SWAP$Output = 'DR2'
  o = SwapRate(SWAP)
  listsDR[[i]] <- o
  
}
listsDR
listsDR1 <- do.call(cbind, listsDR)
# make column name DRCurvature1V
colnames(listsDR1) <- DRCurvature1V


# plot each columns discount rate
# Plotting the matrix
matplot(listsDR1, type = 'l', lty = 1, col = rainbow(N), xlab = "Period", ylab = "Rate")
legend("topright", legend = colnames(listsDR1), col = rainbow(N), lty = 1, cex = 0.8)


source('SWAP Functions.R')
SWAP$Output = 'Value2'
SwapRate(SWAP)
SWAP$DRParm3 = DRCurvature1
#
# Plot swap value and discount rate curvature 1
#
x <- DRCurvature1V
y <- SV
FixYRange = FALSE
if(FixYRange == TRUE){
  MaxYValue = 100
  MinYValue = -100
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Values")
} else {
  mTitle = paste0("Receive Floating Swap Values")
}
xTitle = "Discount Rate Curvature 1"
yTitle = "Swap Values"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue),
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
# lblX <- format.Date(lblX, "%b-%Y")
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
#
# Plot swap rates and discount rate curvature 1
#
x <- DRCurvature1V
y <- SR
FixYRange = FALSE
if(FixYRange == TRUE){
  MaxYValue = 100
  MinYValue = -100
}
if(FixYRange == FALSE){
  MaxYValue = max(y, na.rm = TRUE)
  MinYValue = min(y, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
if(SWAP$SwapType == 1){
  mTitle = paste0("Receive Fixed Swap Rates")
} else {
  mTitle = paste0("Receive Floating Swap Rates")
}
xTitle = "Discount Rate Curvature 1"
yTitle = "Swap Rates"
plot(x, y, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.25)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxXValue) - as.numeric(MinXValue))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinXValue),
  to = as.numeric(MaxXValue), by=IncrementX)), 3)
lblX = TickMarksX
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxYValue) - as.numeric(MinYValue))/8.0
TickMarksY = round(c(seq(from=MinYValue,to=MaxYValue,by=IncrementY)), 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)

beep(sound = 6, print('Finished'))
