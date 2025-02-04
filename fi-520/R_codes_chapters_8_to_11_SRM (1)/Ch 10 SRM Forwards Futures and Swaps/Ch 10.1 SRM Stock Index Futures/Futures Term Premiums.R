# Futures Term Premiums.R
CommodityUnitAdjustment = 1.0 # $ per unit
CommodityUnits = 1.0          # Units per futures contract
FP <- read.xlsx(File, sheet = 1, startRow = 1, colNames = TRUE,
  rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
  rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
FP<-FP[!(FP$Date < StartDate),] # Removes observations prior to SDate
FP<-FP[!(FP$Date > EndDate),] # Removes observations prior to SDate
tail(FP,5) # Show what is in the file
NumberOfObservations = length(FP$C1)
FP$FDSpot <- NA
FP$TP1 <- NA
FP$TP2 <- NA
FP$TP3 <- NA
for(i in 2:NumberOfObservations){
  FP$FDSpot[i] = FP$CSpot[i] - FP$CSpot[i-1]
  FP$TP1[i] = log(FP$C1[i]/FP$CSpot[i])*100
  FP$TP2[i] = log(FP$C2[i]/FP$C1[i])*100 # /(FP$Mat2 - FP$Mat1)
  FP$TP3[i] = log(FP$C3[i]/FP$C2[i])*100 # /(FP$Mat3 - FP$Mat2)
}
FDate = mdy.date(FP$Month, FP$Day, FP$Year)
SpotPrices = CommodityUnitAdjustment*FP$CSpot
FuturesPrices = NumberOfNearbys*NumberOfObservations
FuturesPrices = matrix(FuturesPrices,NumberOfObservations,NumberOfNearbys)
# NEED TO MODIFY IF NumberOfNearbys != 3
FuturesPrices[,1] = CommodityUnitAdjustment*FP$C1
FuturesPrices[,2] = CommodityUnitAdjustment*FP$C2
FuturesPrices[,3] = CommodityUnitAdjustment*FP$C3
#
# Futures Price Plots
#
par(defaultpar) # Reset to default parameters
x <- FDate
xI <- as.integer(x) # Dates are tricky: Convert to integer only (Julian)
y1 <- SpotPrices
y2 <- FuturesPrices[,1]
y3 <- FuturesPrices[,3]
if(FPFixedBounds){
  MaxValueY = FPUpperBound
  MinValueY = FPLowerBound
} else {
  MaxValueY = max(y1, y2, y3, na.rm = TRUE)
  MinValueY = min(y1, y2, y3, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
MaxValueX = max(xI); MinValueX = min(xI)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
legtxt = c("Spot","Nearby","Third Nearby")
mTitle = paste0(Title1," Futures Prices")
xTitle = "Calendar Date"
yTitle = "Futures Price"
lTitle = "Parameter"
# Illustrate more control over plots
#  Want legend outside of box to avoid overwriting data points
par(omi = c(0.2, 0, 0, 0)) # Set outer margins at bottom to 0.2 inches
plot(xI, y1, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(xI, y2, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
lines(xI, y3, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 3, cex = 0.5)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = round((as.numeric(MaxValueX) - as.numeric(MinValueX))/5.0,0)
TickMarksX = c(seq(from = as.numeric(MinValueX), to = as.numeric(MaxValueX),
  by=IncrementX))
lblX = as.Date(TickMarksX, origin = "1960-01-01")
lblX <- format.Date(lblX, "%b-%Y")
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = round((as.numeric(MaxValueY) - as.numeric(MinValueY))/12.0, 1)
TickMarksY = c(seq(from=MinValueY,to=MaxValueY,by=IncrementY))
lblY = paste0(format(round(TickMarksY,0), trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
# Overlay with invisible plot solely for legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", legtxt, horiz = TRUE, cex = 0.75, lwd = c(1,1,1), 
  lty = c(1,1,1), col = c("black","black","black"), bty = "n", pch = c(1, 2, 3),   
  inset = c(0, 0))
par(defaultpar) # Reset to default parameters
#
# Term Premium Plots
#
x <- FDate
xI <- as.integer(x) # Dates are tricky: Convert to integer only (Julian)
MaxValueX = max(xI); MinValueX = min(xI)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
y1 <- FP$RSpot/4 # Convert to quarterly
y2 <- FP$TP1
y3 <- FP$TP3
if(TPFixedBounds){
  MaxValueY = TPUpperBound
  MinValueY = TPLowerBound
} else {
  MaxValueY = max(y1, y2, y3, na.rm = TRUE)
  MinValueY = min(y1, y2, y3, na.rm = TRUE)
}
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
legtxt = c("3M Libor (1/4 Yr)", "Term Premium 1", "Term Premium 3")
mTitle = paste0(Title1," Term Premiums and 3M Libor")
xTitle = "Calendar Date"
yTitle = "Marginal Term Premiums/Rates (Not Annualized)"
lTitle = "Parameter"
# Illustrate more control over plots
# palette() # Check available colors
#  Want legend outside of box to avoid overwriting data points
par(omi = c(0.2, 0, 0, 0)) # Set outer margins at bottom to 0.2 inches
plot(xI, y1, type ="b", main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(xI, y2, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
lines(xI, y3, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 3, cex = 0.5)
box() # create a wrap around the points plotted
# Format x-axis
IncrementX = (as.numeric(MaxValueX) - as.numeric(MinValueX))/5.0
TickMarksX = round(c(seq(from = as.numeric(MinValueX), 
  to = as.numeric(MaxValueX), by=IncrementX)),0)
lblX = as.Date(TickMarksX, origin = "1960-01-01")
lblX <- format.Date(lblX, "%b-%Y")
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY = (as.numeric(MaxValueY) - as.numeric(MinValueY))/12.0
TickMarksY = c(seq(from=MinValueY,to=MaxValueY,by=IncrementY))
lblY = paste0(format(round(TickMarksY,2), trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
# Overlay with invisible plot solely for legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", legtxt, horiz = TRUE, cex = 0.75, lwd = c(1, 1, 1), 
  lty = c(1, 1, 1), col = c("black","black","black"), bty = "n", 
  pch = c(1, 2, 3), inset = c(0, 0))
par(defaultpar) # Reset to default parameters
