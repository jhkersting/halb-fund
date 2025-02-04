# 10.1 Stock Index Futures TP Period Plot to File Test.R
# rmarkdown::render("10.1 Stock Index Futures TP Period Plots to File Test.R", "word_document")
#
# NOTE: Coded for rmarkdown ONLY
#  DO NOT run dev.off()
#  You may not have installed 'Times New Roman'
#   Comment out par(family= ... ) if program fails to run
#
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
# while (!is.null(dev.list()))  dev.off() # Clear old plots
# Times New Roman is not available in PostScript
par(family = 'Times New Roman') # Globally set fonts for graphs
# Technical plot manipulations require resetting back to default
defaultpar <- par() # plot global parameters
Packages <- c("openxlsx", "zoo", "date", "moments", "stats",
  "PerformanceAnalytics", "RColorBrewer", "SDMTools", "weights",
  "xts", "rootSolve", "Weighted.Desc.Stat") # Libraries
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#
# Parameters
#
# File = 'SPX LSC.xlsx'
File = 'MiniSPX LSC.xlsx'
mTitle1 <- 'US S&P 500 (Mini)'
Title1 = as.character('S&P 500')
# File = 'NGComb LSC.xlsx'
# mTitle1 <- 'Natural Gas'
# Title1 = as.character('NG')
FixedYAxis <- TRUE
UpperLimitY <- 20.0
LowerLimitY <- -20.0
StartDate = 20190323 # 19870402 US SPX, 20100319 China (?)
EndDate = 20210323
# sTitle = expression(paste("06/01/1993 through 12/31/2019"))
#
# Plots (Pseudo possible, fix limits--better for analysis)
#  Produces one plot for every day
#  Open all *.jpg files and print to PDF
#  Open PDF and scroll through in full screen mode
# 
PlotPseudoPrices <- FALSE # TRUE: Plot both prices and pseudo prices
FixedLimits <- TRUE # Used for daily plots
# withAutoprint allows graphs in files
withAutoprint({
  Temp <- read.xlsx(File)
  Temp <- Temp[Temp$Date <= EndDate,]
  Temp <- Temp[Temp$Date >= StartDate,]
  FileLength <- length(Temp$Date)
  TempData = data.frame(matrix(vector(), FileLength, 57, dimnames=list(c(), 
    c("Date", "Date1", "Month", "Day", "Year",
      "PsTP1", "PsTP2", "PsTP3", "PsTP4", "PsTP5", "PsTP6", "PsTP7", 
      "PsTP8", "PsTP9", "PsTP10", "PsTP11", "PsTP12", "PsTP13",
      "PsMat1", "PsMat2", "PsMat3", "PsMat4", "PsMat5", "PsMat6", "PsMat7", 
      "PsMat8", "PsMat9", "PsMat10", "PsMat11", "PsMat12", "PsMat13",
      "TP1", "TP2", "TP3", "TP4", "TP5", "TP6", "TP7", 
      "TP8", "TP9", "TP10", "TP11", "TP12", "TP13",
      "Mat1", "Mat2", "Mat3", "Mat4", "Mat5", "Mat6", "Mat7", 
      "Mat8", "Mat9", "Mat10", "Mat11", "Mat12", "Mat13"
    ))), stringsAsFactors=F
  )
  TempData$Date <- Temp$Date
  TempData$Date1 <- Temp$Date1
  TempData$Month <- Temp$Month
  TempData$Day <- Temp$Day
  TempData$Year <- Temp$Year
  TempData$Date <- as.Date(mdy.date(month = TempData$Month, 
    day = TempData$Day, year = TempData$Year))
  TempData$Mat1 <- Temp$Mat1 * 12 # Convert years to months
  TempData$Mat2 <- Temp$Mat2 * 12
  TempData$Mat3 <- Temp$Mat3 * 12
  TempData$Mat4 <- Temp$Mat4 * 12
  TempData$Mat5 <- Temp$Mat5 * 12
  TempData$Mat6 <- Temp$Mat6 * 12
  TempData$Mat7 <- Temp$Mat7 * 12
  TempData$Mat8 <- Temp$Mat8 * 12
  TempData$Mat9 <- Temp$Mat9 * 12
  TempData$Mat10 <- Temp$Mat10 * 12
  TempData$Mat11 <- Temp$Mat11 * 12
  TempData$Mat12 <- Temp$Mat12 * 12
  TempData$TP1 <- Temp$TP1 # Actual nearby prices
  TempData$TP2 <- Temp$TP2
  TempData$TP3 <- Temp$TP3
  TempData$TP4 <- Temp$TP4
  TempData$TP5 <- Temp$TP5
  TempData$TP6 <- Temp$TP6
  TempData$TP7 <- Temp$TP7
  TempData$TP8 <- Temp$TP8
  TempData$TP9 <- Temp$TP9
  TempData$TP10 <- Temp$TP10
  TempData$TP11 <- Temp$TP11
  TempData$TP12 <- Temp$TP12
  TempData$PsMat1 <- 0 # Pseudo months, nearby is spot for pseudo
  TempData$PsMat2 <- 1 
  TempData$PsMat3 <- 2 
  TempData$PsMat4 <- 3 
  TempData$PsMat5 <- 4 
  TempData$PsMat6 <- 5 
  TempData$PsMat7 <- 6 
  TempData$PsMat8 <- 7 
  TempData$PsMat9 <- 8 
  TempData$PsMat10 <- 9 
  TempData$PsMat11 <- 10 
  TempData$PsMat12 <- 11 
  TempData$PsMat13 <- 12 
  TempData$PsTP1 <- Temp$PsTP1 
  TempData$PsTP2 <- Temp$PsTP2
  TempData$PsTP3 <- Temp$PsTP3
  TempData$PsTP4 <- Temp$PsTP4
  TempData$PsTP5 <- Temp$PsTP5
  TempData$PsTP6 <- Temp$PsTP6
  TempData$PsTP7 <- Temp$PsTP7
  TempData$PsTP8 <- Temp$PsTP8
  TempData$PsTP9 <- Temp$PsTP9
  TempData$PsTP10 <- Temp$PsTP10
  TempData$PsTP11 <- Temp$PsTP11
  TempData$PsTP12 <- Temp$PsTP12
  TempData$PsTP13 <- Temp$PsTP13
  # Need maximum and minimum of PsTP1-12 and TP1-12 for charts
  if(PlotPseudoPrices){
    UpperLimit <- max(TempData$TP1, TempData$TP2, TempData$TP3, TempData$TP4, 
      TempData$TP5, TempData$TP6, TempData$TP7, TempData$TP8, 
      TempData$TP9, TempData$TP10, TempData$TP11, TempData$TP12, 
      TempData$PsTP1, TempData$PsTP2, TempData$PsTP3, TempData$PsTP4, 
      TempData$PsTP5, TempData$PsTP6, TempData$PsTP7, TempData$PsTP8, 
      TempData$PsTP9, TempData$PsTP10, TempData$PsTP11, TempData$PsTP12,
      TempData$PsTP13, na.rm = TRUE
    )
    LowerLimit <- min(TempData$TP1, TempData$TP2, TempData$TP3, TempData$TP4, 
      TempData$TP5, TempData$TP6, TempData$TP7, TempData$TP8, 
      TempData$TP9, TempData$TP10, TempData$TP11, TempData$TP12, 
      TempData$PsTP1, TempData$PsTP2, TempData$PsTP3, TempData$PsTP4, 
      TempData$PsTP5, TempData$PsTP6, TempData$PsTP7, TempData$PsTP8, 
      TempData$PsTP9, TempData$PsTP10, TempData$PsTP11, TempData$PsTP12,
      TempData$PsTP13, na.rm = TRUE
    )
  } else {
    UpperLimit <- max(TempData$TP1, TempData$TP2, TempData$TP3, TempData$TP4, 
      TempData$TP5, TempData$TP6, TempData$TP7, TempData$TP8, 
      TempData$TP9, TempData$TP10, TempData$TP11, TempData$TP12, 
      na.rm = TRUE
    )
    LowerLimit <- min(TempData$TP1, TempData$TP2, TempData$TP3, TempData$TP4, 
      TempData$TP5, TempData$TP6, TempData$TP7, TempData$TP8, 
      TempData$TP9, TempData$TP10, TempData$TP11, TempData$TP12, 
      na.rm = TRUE
    )
  }
  PlotFileLength <- 13 
  PlotTempData = data.frame(matrix(vector(), PlotFileLength, 9, 
    dimnames=list(c(), c("Date", "Date1", "Month", "Day", "Year", "PsTP", 
      "PsMat", "TP", "Mat"))), stringsAsFactors=F)
# Convert time series into cross-sectional data (on for each graph)
  for (j in 1:FileLength){
    k = 0 # Observation 1
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat1[j]
    PlotTempData$TP[k+1] <- TempData$TP1[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat1[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP1[j]
    k = k + 1 # Observation 2
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat2[j]
    PlotTempData$TP[k+1] <- TempData$TP2[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat2[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP2[j]
    k = k + 1 # Observation 3
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat3[j]
    PlotTempData$TP[k+1] <- TempData$TP3[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat3[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP3[j]
    k = k + 1 # Observation 4
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat4[j]
    PlotTempData$TP[k+1] <- TempData$TP4[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat4[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP4[j]
    k = k + 1 # Observation 5
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat5[j]
    PlotTempData$TP[k+1] <- TempData$TP5[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat5[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP5[j]
    k = k + 1 # Observation 6
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat6[j]
    PlotTempData$TP[k+1] <- TempData$TP6[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat6[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP6[j]
    k = k + 1 # Observation 7
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat7[j]
    PlotTempData$TP[k+1] <- TempData$TP7[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat7[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP7[j]
    k = k + 1 # Observation 8
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat8[j]
    PlotTempData$TP[k+1] <- TempData$TP8[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat8[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP8[j]
    k = k + 1 # Observation 9
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat9[j]
    PlotTempData$TP[k+1] <- TempData$TP9[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat9[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP9[j]
    k = k + 1 # Observation 10
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat10[j]
    PlotTempData$TP[k+1] <- TempData$TP10[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat10[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP10[j]
    k = k + 1 # Observation 11
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat11[j]
    PlotTempData$TP[k+1] <- TempData$TP11[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat11[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP11[j]
    k = k + 1 # Observation 12
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat12[j]
    PlotTempData$TP[k+1] <- TempData$TP12[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat12[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP12[j]
    k = k + 1 # Observation 13
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat13[j]
    PlotTempData$TP[k+1] <- TempData$TP13[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat13[j]
    PlotTempData$PsTP[k+1] <- TempData$PsTP13[j]
# Work on plot  
# display.brewer.all() # All palette available from RColorBrewer
# First 9 colors in Paired palette (prints well)
# cols contain the names of different colors
    cols<-brewer.pal(n=9, name="Paired") 
    x1 <- PlotTempData$PsMat 
    x2 <- PlotTempData$Mat
    MaxValueX = 12 # Set range to one year
    MinValueX = 0
    xlim1 = c(1:2)
    xlim1[1] = MinValueX
    xlim1[2] = MaxValueX
    IncrementX = (as.numeric(MaxValueX) - as.numeric(MinValueX))/6.0
    TickMarksX = c(seq(from=MinValueX,to=MaxValueX,by=IncrementX))
    lblX = as.character(round(TickMarksX, 3))
    y1 <- PlotTempData$PsTP
    y2 <- PlotTempData$TP
    legtxt = c("Actual Term Premiums", "Pseudo Term Premiums")
    lTitle = "Marginal Term Premiums"
    xTitle = "Months to Maturity"
    yTitle = "Marginal Term Premiums"
    mTitle <- paste0(mTitle1, '\n ', PlotTempData$Month[1], '/', 
      PlotTempData$Day[1], '/', PlotTempData$Year[1])
    if(FixedLimits){
      MaxValueY = UpperLimit
      MinValueY = LowerLimit
    } else {
      MaxValueY = max(y1, y2, na.rm = TRUE)
      MinValueY = min(y1, y2, na.rm = TRUE)
    }
    if(FixedYAxis){
      MaxValueY = UpperLimitY
      MinValueY = LowerLimitY
    }    
    IncrementY = (as.numeric(MaxValueY) - as.numeric(MinValueY))/12.0
    ylim1 = c(1:2)
    ylim1[1] = MinValueY
    ylim1[2] = MaxValueY
    TickMarksY = c(seq(from=MinValueY,to=MaxValueY,by=IncrementY))
    lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4, 
      justify = c("right"), width = 0, big.mark = ","))
# Set plot title to date so each plot is different
    TDate <- as.Date(PlotTempData$Date1[1], origin = "1900-01-01")
    mTitle2 <- as.character(TDate)
# # Required to see plot in file
#     withAutoprint({
      plot(x2, y2, axes = FALSE,
           type = "p", main = mTitle, xlab = xTitle, ylab = yTitle, 
           col = "blue", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.75)
    # }, evaluated = TRUE)
    if(PlotPseudoPrices){
      lines(x1, y1, type = "p", col = "red", xlim = xlim1, ylim = ylim1, 
            pch = 2, cex = 0.75)
    }
    box() # create a wrap around the points plotted
    axis(labels=NA,side=1,tck=-0.015,at=TickMarksX) 
    axis(lwd=0,side=1,line=-0.4,at=TickMarksX, label=lblX) 
    axis(labels=NA,side=2,tck=-0.015, at=TickMarksY)
    axis(lwd=0,line=-0.4,side=2,las=1,at=TickMarksY, label = lblY) 
    if(PlotPseudoPrices){
      legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(0, 0),
        col = c("blue", "red"), pch = c(1, 2), bty = "n", title = lTitle)
    }
  }
}, evaluated = TRUE)

# See A1.6 R Generic Plot Example.R with Generic Plots.R to move legend
# outside of plot
