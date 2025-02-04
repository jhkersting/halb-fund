# Term Premium Plots.R (Support file for SHCA Batch 2018-05-17.R)
# Insight: Generic calculations and plots can be separated from main code
FP <- read.xlsx(File, sheet = 1, startRow = 1, colNames = TRUE,
  rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
  rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
FP <- FP[FP$Date >= StartDate,]
FP <- FP[FP$Date <= EndDate,]
Rates <- FP$RSpot*(FP$Mat2 - FP$Mat1)
Return <- FP$PCSpot
# Marginal term premiums
TP1 = FP$TP1
TP2 = FP$TP2
TP3 = FP$TP3
TP4 = FP$TP4
TP5 = FP$TP5
TP6 = FP$TP6
TP7 = FP$TP7
TP8 = FP$TP8
TP9 = FP$TP9
TP10 = FP$TP10
TP11 = FP$TP11
TP12 = FP$TP12
Date <- mdy.date(FP$Month, FP$Day, FP$Year)
# Drop first observation, which is NA
Rates <- Rates[-1]
Return <- Return[-1]
Date <- Date [-1]
TP1 = TP1[-1]
TP2 = TP2[-1]
TP3 = TP3[-1]
TP4 = TP4[-1]
TP5 = TP5[-1]
TP6 = TP6[-1]
TP7 = TP7[-1]
TP8 = TP8[-1]
TP9 = TP9[-1]
TP10 = TP10[-1]
TP11 = TP11[-1]
TP12 = TP12[-1]
# Convert to time-series using zoo. Return is the series, Date is its index.
Rates <- zoo(Rates, Date)
Return <- zoo(Return, Date)
TP1 <- zoo(TP1,Date)
TP2 <- zoo(TP2,Date)
TP3 <- zoo(TP3,Date)
TP4 <- zoo(TP4,Date)
TP5 <- zoo(TP5,Date)
TP6 <- zoo(TP6,Date)
TP7 <- zoo(TP7,Date)
TP8 <- zoo(TP8,Date)
TP9 <- zoo(TP9,Date)
TP10 <- zoo(TP10,Date)
TP11 <- zoo(TP11,Date)
TP12 <- zoo(TP12,Date)
# Option value plots - more advance plotting
# Work on X-axis
gDate = index(TP1) # Note gDate is Class 'date' int
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
if(TPFixedBounds){
  MaxValueY = TPUpperBound
  MinValueY = TPLowerBound
} else {
  if('0' %in% Plots){
    T1 <- TP1
  } else if('1' %in% Plots){
    T2 <- TP2
  } else if('2' %in% Plots){
    T3 <- TP3
  } else if('3' %in% Plots){
    T4 <- TP4
  } else if('4' %in% Plots){
    T5 <- TP5
  } else if('5' %in% Plots){
    T6 <- TP6
  } else if('6' %in% Plots){
    T7 <- TP7
  } else if('7' %in% Plots){
    T8 <- TP8
  } else if('8' %in% Plots){
    T9 <- TP9
  } else if('9' %in% Plots){
    T10 <- TP10
  } else if('10' %in% Plots){
    T11 <- TP11
  } else if('11' %in% Plots){
    T12 <- TP12
  }
  MaxValueY = max(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,na.rm=TRUE)
  MinValueY = min(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,na.rm=TRUE)
}
ylim1 = c(1:2)
ylim1[1] = MinValueY
ylim1[2] = MaxValueY
IncrementY = (as.numeric(MaxValueY) - as.numeric(MinValueY))/8.0
yTitle = "Marginal Term Premiums"
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
    legtxt <- c('Marginal Term Premium', 'Period Rate')
    lTitle <- 'Items'
    legend("topleft", legtxt, cex = 0.75, lwd = c(1, 2), lty = c(1, 1),
      col = c("black", "black"), pch = c(19, NA_integer_), bty = "n", title = lTitle)
  }
}
# If Plots contains 0, then produce this plot
if('0' %in% Plots){
  mTitle <- paste0(mTitle1, "\nSpot and 1st Nearby")
  plot(index(TP1), as.numeric(TP1), axes = FALSE, type = "l", main = mTitle, 
       xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
       pch = 19, cex = 0.5, lwd = 1)
  FinishPlot()
}
# If Plots contains 1, then produce this plot
if('1' %in% Plots){
  mTitle <- paste0(mTitle1, "\n1st and 2nd Nearbys")
  plot(index(TP1), as.numeric(TP2), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('2' %in% Plots){
  mTitle <- paste0(mTitle1, "\n2nd and 3rd Nearbys")
  plot(index(TP1), as.numeric(TP3), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('3' %in% Plots){
  mTitle <- paste0(mTitle1, "\n3rd and 4th Nearbys")
  plot(index(TP1), as.numeric(TP4), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('4' %in% Plots){
  mTitle <- paste0(mTitle1, "\n4th and 5th Nearbys")
  plot(index(TP1), as.numeric(TP5), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('5' %in% Plots){
  mTitle <- paste0(mTitle1, "\n5th and 6th Nearbys")
  plot(index(TP1), as.numeric(TP6), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('6' %in% Plots){
  mTitle <- paste0(mTitle1, "\n6th and 7th Nearbys")
  plot(index(TP1), as.numeric(TP7), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('7' %in% Plots){
  mTitle <- paste0(mTitle1, "\n7th and 8th Nearbys")
  plot(index(TP1), as.numeric(TP8), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('8' %in% Plots){
  mTitle <- paste0(mTitle1, "\n8th and 9th Nearbys")
  plot(index(TP1), as.numeric(TP9), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('9' %in% Plots){
  mTitle <- paste0(mTitle1, "\n9th and 10th Nearbys")
  plot(index(TP1), as.numeric(TP10), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('10' %in% Plots){
  mTitle <- paste0(mTitle1, "\n10th and 11th Nearbys")
  plot(index(TP1), as.numeric(TP11), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('11' %in% Plots){
  mTitle <- paste0(mTitle1, "\n11th and 12th Nearbys")
  plot(index(TP1), as.numeric(TP12), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
if('12' %in% Plots){
  mTitle <- paste0(mTitle1, "\n12th and 13th Nearbys")
  plot(index(TP1), as.numeric(TP13), axes = FALSE, type = "b", main = mTitle, 
    xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, 
    pch = 19, cex = 0.5, lwd = 1)
  if(IncludeRates){
    lines(index(TP1), as.numeric(Rates), type = "l", col = "black", xlim = xlim1, ylim = ylim1, 
          pch = 21, cex = 0.75)
  }
  FinishPlot()
}
