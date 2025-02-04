# Percentage Change in Futures Plots.R (Support file for SHCA Batch 2018-05-17.R)
# Insight: Generic calculations and plots can be separated from main code
FP <- read.xlsx(File, sheet = 1, startRow = 1, colNames = TRUE,
  rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
  rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
FP <- FP[FP$Date >= StartDate,]
FP <- FP[FP$Date <= EndDate,]
Return <- FP$PCSpot
# Marginal term premiums
PCF1 = FP$PCF1*100.0
PCF2 = FP$PCF2*100.0
PCF3 = FP$PCF3*100.0
PCF4 = FP$PCF4*100.0
PCF5 = FP$PCF5*100.0
PCF6 = FP$PCF6*100.0
PCF7 = FP$PCF7*100.0
PCF8 = FP$PCF8*100.0
PCF9 = FP$PCF9*100.0
PCF10 = FP$PCF10*100.0
PCF11 = FP$PCF11*100.0
PCF12 = FP$PCF12*100.0
Date <- mdy.date(FP$Month, FP$Day, FP$Year)
# Drop first observation, which is NA
Return <- Return[-1]
Date <- Date [-1]
PCF1 = PCF1[-1]
PCF2 = PCF2[-1]
PCF3 = PCF3[-1]
PCF4 = PCF4[-1]
PCF5 = PCF5[-1]
PCF6 = PCF6[-1]
PCF7 = PCF7[-1]
PCF8 = PCF8[-1]
PCF9 = PCF9[-1]
PCF10 = PCF10[-1]
PCF11 = PCF11[-1]
PCF12 = PCF12[-1]
# Convert to time-series using zoo. Return is the series, Date is its index.
Return <- zoo(Return, Date)
PCF1 <- zoo(PCF1,Date)
PCF2 <- zoo(PCF2,Date)
PCF3 <- zoo(PCF3,Date)
PCF4 <- zoo(PCF4,Date)
PCF5 <- zoo(PCF5,Date)
PCF6 <- zoo(PCF6,Date)
PCF7 <- zoo(PCF7,Date)
PCF8 <- zoo(PCF8,Date)
PCF9 <- zoo(PCF9,Date)
PCF10 <- zoo(PCF10,Date)
PCF11 <- zoo(PCF11,Date)
PCF12 <- zoo(PCF12,Date)
# Option value plots - more advance plotting
# Work on X-axis
gDate = index(PCF1) # Note gDate is Class 'date' int
MaxValueX = max(gDate)
MinValueX = min(gDate)
xlim1 = c(1:2)
xlim1[1] = MinValueX # Set limits on X axis
xlim1[2] = MaxValueX
xTitle = "Date"
# Set tick increments for X axis
IncrementX = as.integer(round((as.numeric(MaxValueX) - as.numeric(MinValueX))/7.0,0))
gMinValueX <- MinValueX # Preserve date format
gMaxValueX <- MaxValueX 
# Set X axis tick mark valuues
TickMarksX = c(seq(from=as.integer(gMinValueX),to=as.integer(gMaxValueX),by=IncrementX))
# Convert to date format
TickMarksX <- as.date(TickMarksX) 
lblX = format.Date(TickMarksX, "%Y-%m")
# Y
if(PCFFixedBounds){
  MaxValueY = PCFUpperBound
  MinValueY = PCFLowerBound
} else {
  MaxValueY = max(PCF1,PCF2,PCF3,PCF4,PCF5,PCF6,PCF7,PCF8,PCF9,PCF10,PCF11,PCF12,na.rm=TRUE)
  MinValueY = min(PCF1,PCF2,PCF3,PCF4,PCF5,PCF6,PCF7,PCF8,PCF9,PCF10,PCF11,PCF12,na.rm=TRUE)
}
ylim1 = c(1:2)
ylim1[1] = MinValueY
ylim1[2] = MaxValueY
IncrementY = (as.numeric(MaxValueY) - as.numeric(MinValueY))/8.0
yTitle = "Percentage Change in Futures"
TickMarksY = c(seq(from=MinValueY,to=MaxValueY,by=IncrementY))
TickMarksY = round(TickMarksY, 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 2, 
  justify = c("right"), width = 0, big.mark = ","), "%")
# Function to complete each plot
FinishPlot = function() {
  box() # create a wrap around the points plotted
  axis(labels=NA, side=1, tck=-0.015, at=TickMarksX) # X tick marks only
  axis(lwd=0, side=1, line=-0.4, at=TickMarksX, label=lblX) # X labels
  axis(labels=NA, side=2, tck=-0.015, at=TickMarksY) # Y tick marks only
  axis(lwd=0,line=-0.8, side=2, las=1, at=TickMarksY, label = lblY) # Y labels
}
# If Plots contains 0, then produce this plot
if('0' %in% Plots){
  mTitle <- paste0(mTitle1, "\nSpot")
  plot(index(Return), as.numeric(Return), axes = FALSE, type = "b", main = mTitle, 
       xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
       pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
# If Plots contains 1, then produce this plot
if('1' %in% Plots){
  mTitle <- paste0(mTitle1, "\n1st Nearbys")
  plot(index(PCF1), as.numeric(PCF1), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('2' %in% Plots){
  mTitle <- paste0(mTitle1, "\n2nd Nearbys")
  plot(index(PCF1), as.numeric(PCF2), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('3' %in% Plots){
  mTitle <- paste0(mTitle1, "\n3rd Nearbys")
  plot(index(PCF1), as.numeric(PCF3), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('4' %in% Plots){
  mTitle <- paste0(mTitle1, "\n4th Nearbys")
  plot(index(PCF1), as.numeric(PCF4), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('5' %in% Plots){
  mTitle <- paste0(mTitle1, "\n5th Nearbys")
  plot(index(PCF1), as.numeric(PCF5), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('6' %in% Plots){
  mTitle <- paste0(mTitle1, "\n6th Nearbys")
  plot(index(PCF1), as.numeric(PCF6), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('7' %in% Plots){
  mTitle <- paste0(mTitle1, "\n7th Nearbys")
  plot(index(PCF1), as.numeric(PCF7), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('8' %in% Plots){
  mTitle <- paste0(mTitle1, "\n8th Nearbys")
  plot(index(PCF1), as.numeric(PCF8), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('9' %in% Plots){
  mTitle <- paste0(mTitle1, "\n9th Nearbys")
  plot(index(PCF1), as.numeric(PCF9), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('10' %in% Plots){
  mTitle <- paste0(mTitle1, "\n10th Nearbys")
  plot(index(PCF1), as.numeric(PCF10), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('11' %in% Plots){
  mTitle <- paste0(mTitle1, "\n11th Nearbys")
  plot(index(PCF1), as.numeric(PCF11), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('12' %in% Plots){
  mTitle <- paste0(mTitle1, "\n12th Nearbys")
  plot(index(PCF1), as.numeric(PCF12), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
