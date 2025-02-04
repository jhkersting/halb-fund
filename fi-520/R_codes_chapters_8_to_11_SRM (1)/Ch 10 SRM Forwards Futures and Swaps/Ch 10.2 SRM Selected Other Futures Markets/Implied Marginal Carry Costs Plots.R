# Term Premium Plots.R (Support file for SHCA Batch 2018-05-17.R)
# Insight: Generic calculations and plots can be separated from main code
FP <- read.xlsx(File, sheet = 1, startRow = 1, colNames = TRUE,
  rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
  rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
FP <- FP[FP$Date >= StartDate,]
FP <- FP[FP$Date <= EndDate,]
Rates <- FP$RSpot
Return <- FP$PCSpot
# Annualized Marginal Incremental Carry Costs
IMCC1 = FP$TP1/FP$Mat1 - FP$RSpot
IMCC2 = FP$TP2/(FP$Mat2 - FP$Mat1) - FP$RSpot
IMCC3 = FP$TP3/(FP$Mat3 - FP$Mat2) - FP$RSpot
IMCC4 = FP$TP4/(FP$Mat4 - FP$Mat3) - FP$RSpot
IMCC5 = FP$TP5/(FP$Mat5 - FP$Mat4) - FP$RSpot
IMCC6 = FP$TP6/(FP$Mat6 - FP$Mat5) - FP$RSpot
IMCC7 = FP$TP7/(FP$Mat7 - FP$Mat6) - FP$RSpot
IMCC8 = FP$TP8/(FP$Mat8 - FP$Mat7) - FP$RSpot
IMCC9 = FP$TP9/(FP$Mat9 - FP$Mat8) - FP$RSpot
IMCC10 = FP$TP10/(FP$Mat10 - FP$Mat9) - FP$RSpot
IMCC11 = FP$TP11/(FP$Mat11 - FP$Mat10) - FP$RSpot
IMCC12 = FP$TP12/(FP$Mat12 - FP$Mat11) - FP$RSpot
Date <- mdy.date(FP$Month, FP$Day, FP$Year)
# Drop first observation, which is NA
Rates <- Rates[-1]
Return <- Return[-1]
Date <- Date [-1]
IMCC1 = IMCC1[-1]
IMCC2 = IMCC2[-1]
IMCC3 = IMCC3[-1]
IMCC4 = IMCC4[-1]
IMCC5 = IMCC5[-1]
IMCC6 = IMCC6[-1]
IMCC7 = IMCC7[-1]
IMCC8 = IMCC8[-1]
IMCC9 = IMCC9[-1]
IMCC10 = IMCC10[-1]
IMCC11 = IMCC11[-1]
IMCC12 = IMCC12[-1]
# Convert to time-series using zoo. Return is the series, Date is its index.
Rates <- zoo(Rates, Date)
Return <- zoo(Return, Date)
IMCC1 <- zoo(IMCC1,Date); T1 <- zoo(0,Date) # Ts for plot labels only
IMCC2 <- zoo(IMCC2,Date); T2 <- zoo(0,Date)
IMCC3 <- zoo(IMCC3,Date); T3 <- zoo(0,Date)
IMCC4 <- zoo(IMCC4,Date); T4 <- zoo(0,Date)
IMCC5 <- zoo(IMCC5,Date); T5 <- zoo(0,Date)
IMCC6 <- zoo(IMCC6,Date); T6 <- zoo(0,Date)
IMCC7 <- zoo(IMCC7,Date); T7 <- zoo(0,Date)
IMCC8 <- zoo(IMCC8,Date); T8 <- zoo(0,Date)
IMCC9 <- zoo(IMCC9,Date); T9 <- zoo(0,Date)
IMCC10 <- zoo(IMCC10,Date); T10 <- zoo(0,Date)
IMCC11 <- zoo(IMCC11,Date); T11 <- zoo(0,Date)
IMCC12 <- zoo(IMCC12,Date); T12 <- zoo(0,Date)
# Option value plots - more advance plotting
# Work on X-axis
gDate = index(IMCC1) # Note gDate is Class 'date' int
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
if(IMCCFixedBounds){
  MaxValueY = IMCCUpperBound
  MinValueY = IMCCLowerBound
} else {
  if('0' %in% Plots){
    T1 <- IMCC1
  } else if('1' %in% Plots){
    T2 <- IMCC2
  } else if('2' %in% Plots){
    T3 <- IMCC3
  } else if('3' %in% Plots){
    T4 <- IMCC4
  } else if('4' %in% Plots){
    T5 <- IMCC5
  } else if('5' %in% Plots){
    T6 <- IMCC6
  } else if('6' %in% Plots){
    T7 <- IMCC7
  } else if('7' %in% Plots){
    T8 <- IMCC8
  } else if('8' %in% Plots){
    T9 <- IMCC9
  } else if('9' %in% Plots){
    T10 <- IMCC10
  } else if('10' %in% Plots){
    T11 <- IMCC11
  } else if('11' %in% Plots){
    T12 <- IMCC12
  }
  MaxValueY = max(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,na.rm=TRUE)
  MinValueY = min(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,na.rm=TRUE)
  if(IncludeRates){
    MaxValueY = max(MaxValueY, Rates, na.rm=TRUE)
    MinValueY = min(MinValueY, Rates, na.rm=TRUE)
  }
}
ylim1 = c(1:2)
ylim1[1] = MinValueY
ylim1[2] = MaxValueY
IncrementY = (as.numeric(MaxValueY) - as.numeric(MinValueY))/8.0
yTitle = "Marginal Incremental Carry Cost"
TickMarksY = c(seq(from=MinValueY,to=MaxValueY,by=IncrementY))
TickMarksY = round(TickMarksY, 2)
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 3, 
  justify = c("right"), width = 0, big.mark = ","), "%")
# Function to complete each plot
FinishPlot = function() {
  box() # create a wrap around the points plotted
  axis(labels=NA, side=1, tck=-0.015, at=TickMarksX) # X tick marks only
  axis(lwd=0, side=1, line=-0.4, at=TickMarksX, label=lblX) # X labels
  axis(labels=NA, side=2, tck=-0.015, at=TickMarksY) # Y tick marks only
  axis(lwd=0,line=-0.8, side=2, las=1, at=TickMarksY, label = lblY) # Y labels
  if(IncludeRates){
    legtxt <- c('Marginal Incremental Carry Costs', 'Annual Interest Rate')
    lTitle <- 'Items'
    legend("topleft", legtxt, cex = 0.75, lwd = c(1, 2), lty = c(1, 1),
      col = c("black", "black"), pch = c(19, NA_integer_), bty = "n", 
      title = lTitle)
  }
}
# If Plots contains 0, then produce this plot
if('0' %in% Plots){
  mTitle <- paste0(mTitle1, "\nSpot and 1st Nearby")
  plot(index(IMCC1), as.numeric(IMCC1), axes = FALSE, type = "l", main = mTitle, 
       xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
       pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
# If Plots contains 1, then produce this plot
if('1' %in% Plots){
  mTitle <- paste0(mTitle1, "\n1st and 2nd Nearbys")
  plot(index(IMCC1), as.numeric(IMCC2), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('2' %in% Plots){
  mTitle <- paste0(mTitle1, "\n2nd and 3rd Nearbys")
  plot(index(IMCC1), as.numeric(IMCC3), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('3' %in% Plots){
  mTitle <- paste0(mTitle1, "\n3rd and 4th Nearbys")
  plot(index(IMCC1), as.numeric(IMCC4), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('4' %in% Plots){
  mTitle <- paste0(mTitle1, "\n4th and 5th Nearbys")
  plot(index(IMCC1), as.numeric(IMCC5), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('5' %in% Plots){
  mTitle <- paste0(mTitle1, "\n5th and 6th Nearbys")
  plot(index(IMCC1), as.numeric(IMCC6), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('6' %in% Plots){
  mTitle <- paste0(mTitle1, "\n6th and 7th Nearbys")
  plot(index(IMCC1), as.numeric(IMCC7), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('7' %in% Plots){
  mTitle <- paste0(mTitle1, "\n7th and 8th Nearbys")
  plot(index(IMCC1), as.numeric(IMCC8), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('8' %in% Plots){
  mTitle <- paste0(mTitle1, "\n8th and 9th Nearbys")
  plot(index(IMCC1), as.numeric(IMCC9), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('9' %in% Plots){
  mTitle <- paste0(mTitle1, "\n9th and 10th Nearbys")
  plot(index(IMCC1), as.numeric(IMCC10), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('10' %in% Plots){
  mTitle <- paste0(mTitle1, "\n10th and 11th Nearbys")
  plot(index(IMCC1), as.numeric(IMCC11), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('11' %in% Plots){
  mTitle <- paste0(mTitle1, "\n11th and 12th Nearbys")
  plot(index(IMCC1), as.numeric(IMCC12), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('12' %in% Plots){
  mTitle <- paste0(mTitle1, "\n12th and 13th Nearbys")
  plot(index(IMCC1), as.numeric(IMCC13), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(IMCC1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
      pch = 21, cex = 0.75)
  }
  FinishPlot()
}
