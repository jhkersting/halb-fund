# 10.1 Stock Index Futures Period Plot to File Test.R
# rmarkdown::render("10.1 Stock Index Futures Period Plots to File Test.R", "word_document")
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
  "PerformanceAnalytics", "RColorBrewer", "weights",
  "xts", "rootSolve", "Weighted.Desc.Stat",
  "Hmisc") # Libraries ("SDMTools" removed, now unsupported)
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
StartDate = 20190323 # 19870402 US SPX, 20100319 China (?)
EndDate = 20210323
# sTitle = expression(paste("06/01/1993 through 12/31/2019"))
#
# Plots (Pseudo possible, fix limits--better for analysis)
#  Produces one plot for every day
#  Open all *.jpg files and print to PDF
#  Open PDF and scroll through in full screen mode
# 
PlotPseudoPrices <- TRUE # TRUE: Plot both prices and pseudo prices
FixedLimits <- TRUE # Used for daily plots
# withAutoprint allows graphs in files
withAutoprint({
  Temp <- read.xlsx(File)
  Temp <- Temp[Temp$Date <= EndDate,]
  Temp <- Temp[Temp$Date >= StartDate,]
  FileLength <- length(Temp$Date)
  TempData = data.frame(matrix(vector(), FileLength, 57, dimnames=list(c(), 
    c("Date", "Date1", "Month", "Day", "Year",
      "PsC1", "PsC2", "PsC3", "PsC4", "PsC5", "PsC6", "PsC7", 
      "PsC8", "PsC9", "PsC10", "PsC11", "PsC12", "PsC13",
      "PsMat1", "PsMat2", "PsMat3", "PsMat4", "PsMat5", "PsMat6", "PsMat7", 
      "PsMat8", "PsMat9", "PsMat10", "PsMat11", "PsMat12", "PsMat13",
      "C1", "C2", "C3", "C4", "C5", "C6", "C7", 
      "C8", "C9", "C10", "C11", "C12", "C13",
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
  TempData$C1 <- Temp$C1 # Actual nearby prices
  TempData$C2 <- Temp$C2
  TempData$C3 <- Temp$C3
  TempData$C4 <- Temp$C4
  TempData$C5 <- Temp$C5
  TempData$C6 <- Temp$C6
  TempData$C7 <- Temp$C7
  TempData$C8 <- Temp$C8
  TempData$C9 <- Temp$C9
  TempData$C10 <- Temp$C10
  TempData$C11 <- Temp$C11
  TempData$C12 <- Temp$C12
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
  TempData$PsC1 <- Temp$PsC1 # Estimated pseudo prices
  TempData$PsC2 <- Temp$PsC2
  TempData$PsC3 <- Temp$PsC3
  TempData$PsC4 <- Temp$PsC4
  TempData$PsC5 <- Temp$PsC5
  TempData$PsC6 <- Temp$PsC6
  TempData$PsC7 <- Temp$PsC7
  TempData$PsC8 <- Temp$PsC8
  TempData$PsC9 <- Temp$PsC9
  TempData$PsC10 <- Temp$PsC10
  TempData$PsC11 <- Temp$PsC11
  TempData$PsC12 <- Temp$PsC12
  TempData$PsC13 <- Temp$PsC13
  # Need maximum and minimum of PsC1-12 and C1-12 for charts
  if(PlotPseudoPrices){
    UpperLimit <- max(TempData$C1, TempData$C2, TempData$C3, TempData$C4, 
      TempData$C5, TempData$C6, TempData$C7, TempData$C8, 
      TempData$C9, TempData$C10, TempData$C11, TempData$C12, 
      TempData$PsC1, TempData$PsC2, TempData$PsC3, TempData$PsC4, 
      TempData$PsC5, TempData$PsC6, TempData$PsC7, TempData$PsC8, 
      TempData$PsC9, TempData$PsC10, TempData$PsC11, TempData$PsC12,
      TempData$PsC13, na.rm = TRUE
    )
    LowerLimit <- min(TempData$C1, TempData$C2, TempData$C3, TempData$C4, 
      TempData$C5, TempData$C6, TempData$C7, TempData$C8, 
      TempData$C9, TempData$C10, TempData$C11, TempData$C12, 
      TempData$PsC1, TempData$PsC2, TempData$PsC3, TempData$PsC4, 
      TempData$PsC5, TempData$PsC6, TempData$PsC7, TempData$PsC8, 
      TempData$PsC9, TempData$PsC10, TempData$PsC11, TempData$PsC12,
      TempData$PsC13, na.rm = TRUE
    )
  } else {
    UpperLimit <- max(TempData$C1, TempData$C2, TempData$C3, TempData$C4, 
      TempData$C5, TempData$C6, TempData$C7, TempData$C8, 
      TempData$C9, TempData$C10, TempData$C11, TempData$C12, 
      na.rm = TRUE
    )
    LowerLimit <- min(TempData$C1, TempData$C2, TempData$C3, TempData$C4, 
      TempData$C5, TempData$C6, TempData$C7, TempData$C8, 
      TempData$C9, TempData$C10, TempData$C11, TempData$C12, 
      na.rm = TRUE
    )
  }
  PlotFileLength <- 13 
  PlotTempData = data.frame(matrix(vector(), PlotFileLength, 9, 
    dimnames=list(c(), c("Date", "Date1", "Month", "Day", "Year", "PsC", 
      "PsMat", "C", "Mat"))), stringsAsFactors=F)
# Convert time series into cross-sectional data (on for each graph)
  for (j in 1:FileLength){
    k = 0 # Observation 1
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat1[j]
    PlotTempData$C[k+1] <- TempData$C1[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat1[j]
    PlotTempData$PsC[k+1] <- TempData$PsC1[j]
    k = k + 1 # Observation 2
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat2[j]
    PlotTempData$C[k+1] <- TempData$C2[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat2[j]
    PlotTempData$PsC[k+1] <- TempData$PsC2[j]
    k = k + 1 # Observation 3
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat3[j]
    PlotTempData$C[k+1] <- TempData$C3[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat3[j]
    PlotTempData$PsC[k+1] <- TempData$PsC3[j]
    k = k + 1 # Observation 4
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat4[j]
    PlotTempData$C[k+1] <- TempData$C4[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat4[j]
    PlotTempData$PsC[k+1] <- TempData$PsC4[j]
    k = k + 1 # Observation 5
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat5[j]
    PlotTempData$C[k+1] <- TempData$C5[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat5[j]
    PlotTempData$PsC[k+1] <- TempData$PsC5[j]
    k = k + 1 # Observation 6
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat6[j]
    PlotTempData$C[k+1] <- TempData$C6[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat6[j]
    PlotTempData$PsC[k+1] <- TempData$PsC6[j]
    k = k + 1 # Observation 7
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat7[j]
    PlotTempData$C[k+1] <- TempData$C7[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat7[j]
    PlotTempData$PsC[k+1] <- TempData$PsC7[j]
    k = k + 1 # Observation 8
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat8[j]
    PlotTempData$C[k+1] <- TempData$C8[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat8[j]
    PlotTempData$PsC[k+1] <- TempData$PsC8[j]
    k = k + 1 # Observation 9
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat9[j]
    PlotTempData$C[k+1] <- TempData$C9[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat9[j]
    PlotTempData$PsC[k+1] <- TempData$PsC9[j]
    k = k + 1 # Observation 10
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat10[j]
    PlotTempData$C[k+1] <- TempData$C10[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat10[j]
    PlotTempData$PsC[k+1] <- TempData$PsC10[j]
    k = k + 1 # Observation 11
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat11[j]
    PlotTempData$C[k+1] <- TempData$C11[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat11[j]
    PlotTempData$PsC[k+1] <- TempData$PsC11[j]
    k = k + 1 # Observation 12
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat12[j]
    PlotTempData$C[k+1] <- TempData$C12[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat12[j]
    PlotTempData$PsC[k+1] <- TempData$PsC12[j]
    k = k + 1 # Observation 13
    PlotTempData$Date[k+1] <- TempData$Date[j]
    PlotTempData$Date1[k+1] <- TempData$Date1[j]
    PlotTempData$Month[k+1] <- TempData$Month[j]
    PlotTempData$Day[k+1] <- TempData$Day[j]
    PlotTempData$Year[k+1] <- TempData$Year[j]
    PlotTempData$Mat[k+1] <- TempData$Mat13[j]
    PlotTempData$C[k+1] <- TempData$C13[j]
    PlotTempData$PsMat[k+1] <- TempData$PsMat13[j]
    PlotTempData$PsC[k+1] <- TempData$PsC13[j]
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
    y1 <- PlotTempData$PsC
    y2 <- PlotTempData$C
    legtxt = c("Actual Price", "Pseudo Price")
    lTitle = "Futures Prices"
    xTitle = "Months to Maturity"
    yTitle = "Prices"
    mTitle <- paste0(mTitle1, '\n ', PlotTempData$Month[1], '/', 
      PlotTempData$Day[1], '/', PlotTempData$Year[1])
    if(FixedLimits){
      MaxValueY = UpperLimit
      MinValueY = LowerLimit
    } else {
      MaxValueY = max(y1, y2, na.rm = TRUE)
      MinValueY = min(y1, y2, na.rm = TRUE)
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
    # mTitle2 <- as.character(TDate)
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
