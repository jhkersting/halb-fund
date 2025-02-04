# Period Plots.R
#  Support file for 10.1 Stock Index Futures Test.R

withAutoprint({
  
FileLocation <- paste(File, sep = "")
Temp <- read.xlsx(FileLocation)
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
  ))), stringsAsFactors=F)
TempData$Date <- Temp$Date
TempData$Date1 <- Temp$Date1
TempData$Month <- Temp$Month
TempData$Day <- Temp$Day
TempData$Year <- Temp$Year
TempData$Date <- as.Date(mdy.date(month = TempData$Month, day = TempData$Day, 
  year = TempData$Year))
TempData$Mat1 <- Temp$Mat1 * 12 # Months
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
TempData$C1 <- Temp$C1
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
TempData$PsMat1 <- 0 #Temp$PsMat1
TempData$PsMat2 <- 1 #Temp$PsMat2
TempData$PsMat3 <- 2 #Temp$PsMat3
TempData$PsMat4 <- 3 #Temp$PsMat4
TempData$PsMat5 <- 4 #Temp$PsMat5
TempData$PsMat6 <- 5 #Temp$PsMat6
TempData$PsMat7 <- 6 #Temp$PsMat7
TempData$PsMat8 <- 7 #Temp$PsMat8
TempData$PsMat9 <- 8 #Temp$PsMat9
TempData$PsMat10 <- 9 #Temp$PsMat10
TempData$PsMat11 <- 10 #Temp$PsMat11
TempData$PsMat12 <- 11 #Temp$PsMat12
TempData$PsMat13 <- 12 #Temp$PsMat12
TempData$PsC1 <- Temp$PsC1
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
# Need maximum and minimum of PsC1-12 and C1-12
if(PlotPseudoPrices){
  UpperLimit <- max(TempData$C1, TempData$C2, TempData$C3, TempData$C4, TempData$C5, TempData$C6, 
    TempData$C7, TempData$C8, TempData$C9, TempData$C10, TempData$C11, TempData$C12, 
    TempData$PsC1, TempData$PsC2, TempData$PsC3, TempData$PsC4, TempData$PsC5, TempData$PsC6, 
    TempData$PsC7, TempData$PsC8, TempData$PsC9, TempData$PsC10, TempData$PsC11, TempData$PsC12,
    TempData$PsC13, na.rm = TRUE
  )
  LowerLimit <- min(TempData$C1, TempData$C2, TempData$C3, TempData$C4, TempData$C5, TempData$C6, 
    TempData$C7, TempData$C8, TempData$C9, TempData$C10, TempData$C11, TempData$C12, 
    TempData$PsC1, TempData$PsC2, TempData$PsC3, TempData$PsC4, TempData$PsC5, TempData$PsC6, 
    TempData$PsC7, TempData$PsC8, TempData$PsC9, TempData$PsC10, TempData$PsC11, TempData$PsC12,
    TempData$PsC13, na.rm = TRUE
  )
} else {
  UpperLimit <- max(TempData$C1, TempData$C2, TempData$C3, TempData$C4, TempData$C5, TempData$C6, 
    TempData$C7, TempData$C8, TempData$C9, TempData$C10, TempData$C11, TempData$C12, 
    na.rm = TRUE
  )
  LowerLimit <- min(TempData$C1, TempData$C2, TempData$C3, TempData$C4, TempData$C5, TempData$C6, 
    TempData$C7, TempData$C8, TempData$C9, TempData$C10, TempData$C11, TempData$C12, 
    na.rm = TRUE
  )
}
PlotFileLength <- 13 #FileLength * 12
PlotTempData = data.frame(matrix(vector(), PlotFileLength, 9, dimnames=list(c(), 
  c("Date", "Date1", "Month", "Day", "Year", "PsC", "PsMat", "C", "Mat"
  ))), stringsAsFactors=F)
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
  #display.brewer.all() # All palette available from RColorBrewer
  cols<-brewer.pal(n=9, name="Paired") # First 9 colors in Paired palette (prints well)
  #cols #cols contain the names of four different colors
  x1 <- PlotTempData$PsMat 
  x2 <- PlotTempData$Mat
  # MaxValueX = max(x1) # Set range to pseudo maturities
  # MinValueX = min(x1)
  MaxValueX = 12 # Set range to one year
  MinValueX = 0
  xlim1 = c(1:2)
  xlim1[1] = MinValueX
  xlim1[2] = MaxValueX
  IncrementX = (as.numeric(MaxValueX) - as.numeric(MinValueX))/6.0
  TickMarksX = c(seq(from=MinValueX,to=MaxValueX,by=IncrementX))
  lblX = as.character(round(TickMarksX, 3))
  # Arb1: Nearby - nth Nearby (6 or 12) ratio of RSD to Average RSD (Normalized)
  y1 <- PlotTempData$PsC
  y2 <- PlotTempData$C
  legtxt = c("Actual Price", "Pseudo Price")
  lTitle = "Futures Prices"
  xTitle = "Months to Maturity"
  yTitle = "Prices"
  mTitle <- paste0(mTitle1, '\n ', PlotTempData$Month[1], '/', PlotTempData$Day[1], 
    '/', PlotTempData$Year[1])
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
  
  # mTitle2 <- paste0(PlotTempData$Year[1],PlotTempData$Month[1], 
  #   PlotTempData$Day[1])
  
  TDate <- as.Date(PlotTempData$Date1[1], origin = "1900-01-01")
  mTitle2 <- as.character(TDate)
  
  # PlotName <- file.path(paste(mTitle2, ".jpg", sep = ""))
  # jpeg(file=PlotName) # Produce plots in files
  
  withAutoprint({
    
    plot(x2, y2, axes = FALSE,
         type = "p", main = mTitle, xlab = xTitle, ylab = yTitle, col = "blue", 
         xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.75)
    
    
  }, evaluated = TRUE)
  
  
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
  
  
  # dev.off() # Shuts down producing plots in files
  
  
}

}, evaluated = TRUE)


