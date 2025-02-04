# Marginal Contribution Plots.R (Support file for ....R)
# Insight: Generic calculations and plots can be separated from main code
FP <- read.xlsx(File, sheet = 1, startRow = 1, colNames = TRUE,
  rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
  rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
FP <- FP[FP$Date >= StartDate,]
FP <- FP[FP$Date <= EndDate,]
Return <- FP$PCSpot
# Marginal contributions
MC1 = 100*(FP$PCSpot - FP$PCF1)
MC2 = 100*(FP$PCF2 - FP$PCF1)
MC3 = 100*(FP$PCF3 - FP$PCF2)
MC4 = 100*(FP$PCF4 - FP$PCF3)
MC5 = 100*(FP$PCF5 - FP$PCF4)
MC6 = 100*(FP$PCF6 - FP$PCF5)
MC7 = 100*(FP$PCF7 - FP$PCF6)
MC8 = 100*(FP$PCF8 - FP$PCF7)
MC9 = 100*(FP$PCF9 - FP$PCF8)
MC10 = 100*(FP$PCF10 - FP$PCF9)
MC11 = 100*(FP$PCF11 - FP$PCF10)
MC12 = 100*(FP$PCF12 - FP$PCF11)

Date <- mdy.date(FP$Month, FP$Day, FP$Year)
# Drop first observation, which is NA
Return <- Return[-1]
Date <- Date [-1]
MC1 = MC1[-1]
MC2 = MC2[-1]
MC3 = MC3[-1]
MC4 = MC4[-1]
MC5 = MC5[-1]
MC6 = MC6[-1]
MC7 = MC7[-1]
MC8 = MC8[-1]
MC9 = MC9[-1]
MC10 = MC10[-1]
MC11 = MC11[-1]
MC12 = MC12[-1]
# Convert to time-series using zoo. Return is the series, Date is its index.
Return <- zoo(Return, Date)
MC1 <- zoo(MC1,Date); T1 <- zoo(0,Date) # Ts for plot labels only
MC2 <- zoo(MC2,Date); T2 <- zoo(0,Date)
MC3 <- zoo(MC3,Date); T3 <- zoo(0,Date)
MC4 <- zoo(MC4,Date); T4 <- zoo(0,Date)
MC5 <- zoo(MC5,Date); T5 <- zoo(0,Date)
MC6 <- zoo(MC6,Date); T6 <- zoo(0,Date)
MC7 <- zoo(MC7,Date); T7 <- zoo(0,Date)
MC8 <- zoo(MC8,Date); T8 <- zoo(0,Date)
MC9 <- zoo(MC9,Date); T9 <- zoo(0,Date)
MC10 <- zoo(MC10,Date); T10 <- zoo(0,Date)
MC11 <- zoo(MC11,Date); T11 <- zoo(0,Date)
MC12 <- zoo(MC12,Date); T12 <- zoo(0,Date)
# Option value plots - more advance plotting
# Work on X-axis
gDate = index(MC1) # Note gDate is Class 'date' int
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
if(MCFixedBounds){
  MaxValueY = MCUpperBound
  MinValueY = MCLowerBound
} else {
  if('0' %in% Plots){
    T1 <- MC1
  } else if('1' %in% Plots){
    T2 <- MC2
  } else if('2' %in% Plots){
    T3 <- MC3
  } else if('3' %in% Plots){
    T4 <- MC4
  } else if('4' %in% Plots){
    T5 <- MC5
  } else if('5' %in% Plots){
    T6 <- MC6
  } else if('6' %in% Plots){
    T7 <- MC7
  } else if('7' %in% Plots){
    T8 <- MC8
  } else if('8' %in% Plots){
    T9 <- MC9
  } else if('9' %in% Plots){
    T10 <- MC10
  } else if('10' %in% Plots){
    T11 <- MC11
  } else if('11' %in% Plots){
    T12 <- MC12
  }
  MaxValueY = max(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,na.rm=TRUE)
  MinValueY = min(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,na.rm=TRUE)
}
ylim1 = c(1:2)
ylim1[1] = MinValueY
ylim1[2] = MaxValueY
IncrementY = (as.numeric(MaxValueY) - as.numeric(MinValueY))/8.0
yTitle = "Marginal Contribution"
TickMarksY = c(seq(from=MinValueY,to=MaxValueY,by=IncrementY))
TickMarksY = round(TickMarksY, 3)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 3, 
  justify = c("right"), width = 0, big.mark = ","), "%")
# Function to complete each plot
FinishPlot = function() {
  box() # create a wrap around the points plotted
  axis(labels=NA, side=1, tck=-0.015, at=TickMarksX) # X tick marks only
  axis(lwd=0, side=1, line=-0.4, at=TickMarksX, label=lblX) # X labels
  axis(labels=NA, side=2, tck=-0.015, at=TickMarksY) # Y tick marks only
  axis(lwd=0,line=-0.8, side=2, las=1, at=TickMarksY, label = lblY) # Y labels
}
# If Plots contains 1, then produce this plot
if('0' %in% Plots){
  mTitle <- paste0(mTitle1, "\nSpot and 1st Nearby")
  plot(index(MC1), as.numeric(MC1), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('1' %in% Plots){
  mTitle <- paste0(mTitle1, "\n1st and 2nd Nearbys")
  plot(index(MC1), as.numeric(MC2), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('2' %in% Plots){
  mTitle <- paste0(mTitle1, "\n2nd and 3rd Nearbys")
  plot(index(MC1), as.numeric(MC3), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('3' %in% Plots){
  mTitle <- paste0(mTitle1, "\n3rd and 4th Nearbys")
  plot(index(MC1), as.numeric(MC4), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('4' %in% Plots){
  mTitle <- paste0(mTitle1, "\n4th and 5th Nearbys")
  plot(index(MC1), as.numeric(MC5), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('5' %in% Plots){
  mTitle <- paste0(mTitle1, "\n5th and 6th Nearbys")
  plot(index(MC1), as.numeric(MC6), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('6' %in% Plots){
  mTitle <- paste0(mTitle1, "\n6th and 7th Nearbys")
  plot(index(MC1), as.numeric(MC7), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('7' %in% Plots){
  mTitle <- paste0(mTitle1, "\n7th and 8th Nearbys")
  plot(index(MC1), as.numeric(MC8), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('8' %in% Plots){
  mTitle <- paste0(mTitle1, "\n8th and 9th Nearbys")
  plot(index(MC1), as.numeric(MC9), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('9' %in% Plots){
  mTitle <- paste0(mTitle1, "\n9th and 10th Nearbys")
  plot(index(MC1), as.numeric(MC10), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('10' %in% Plots){
  mTitle <- paste0(mTitle1, "\n10th and 11th Nearbys")
  plot(index(MC1), as.numeric(MC11), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
if('11' %in% Plots){
  mTitle <- paste0(mTitle1, "\n11th and 12th Nearbys")
  plot(index(MC1), as.numeric(MC12), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
