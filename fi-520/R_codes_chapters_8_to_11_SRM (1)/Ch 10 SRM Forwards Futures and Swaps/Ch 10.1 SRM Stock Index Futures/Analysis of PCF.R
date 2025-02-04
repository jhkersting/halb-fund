# Analysis of PCF.R 
#
# Analysis of percentage change in futures prices
#
FileLocation <- paste(File, sep = "")
Temp <- read.xlsx(FileLocation)
Temp <- Temp[Temp$Date <= EndDate,]
Temp <- Temp[Temp$Date >= StartDate,]
FileLength <- length(Temp$Date)
TempData = data.frame(matrix(vector(), FileLength, 22, dimnames=list(c(), 
  c("Date", "Date1", "Month", "Day", "Year",
    "PsPCF1", "PsPCF2", "PsPCF3", "PsPCF4", "PsPCF5", "PsPCF6", "PsPCF7", 
    "PsPCF8", "PsPCF9", "PsPCF10", "PsPCF11", "PsPCF12",
    "PsTP2", "PsTP3", "PsMC2", "PsMC3", "PCSlope"
  ))), stringsAsFactors=F)
TempData$Date <- Temp$Date
TempData$Date1 <- Temp$Date1
TempData$Month <- Temp$Month
TempData$Day <- Temp$Day
TempData$Year <- Temp$Year
TempData$Date <- as.Date(mdy.date(month = TempData$Month, day = TempData$Day, 
  year = TempData$Year))
TempData$PsPCF1 <- Temp$PsPCF1
TempData$PsPCF2 <- Temp$PsPCF2
TempData$PsPCF3 <- Temp$PsPCF3
TempData$PsPCF4 <- Temp$PsPCF4
TempData$PsPCF5 <- Temp$PsPCF5
TempData$PsPCF6 <- Temp$PsPCF6
TempData$PsPCF7 <- Temp$PsPCF7
TempData$PsPCF8 <- Temp$PsPCF8
TempData$PsPCF9 <- Temp$PsPCF9
TempData$PsPCF10 <- Temp$PsPCF10
TempData$PsPCF11 <- Temp$PsPCF11
TempData$PsPCF12 <- Temp$PsPCF12

TempData$Arb3 <- Temp$PCSlope
TempData$Arb4 <- Temp$PsTP2
TempData$Arb5 <- Temp$PsTP3
TempData$Arb6 <- Temp$PsMC2
TempData$Arb7 <- Temp$PsMC3
#rm(Temp) 
# Rolling weighted standard deviations



RSD1 <- rollapply(as.numeric(TempData$PsPCF1), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD2 <- rollapply(as.numeric(TempData$PsPCF2), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD3 <- rollapply(as.numeric(TempData$PsPCF3), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD4 <- rollapply(as.numeric(TempData$PsPCF4), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD5 <- rollapply(as.numeric(TempData$PsPCF5), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD6 <- rollapply(as.numeric(TempData$PsPCF6), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD7 <- rollapply(as.numeric(TempData$PsPCF7), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD8 <- rollapply(as.numeric(TempData$PsPCF8), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD9 <- rollapply(as.numeric(TempData$PsPCF9), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD10 <- rollapply(as.numeric(TempData$PsPCF10), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD11 <- rollapply(as.numeric(TempData$PsPCF11), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD12 <- rollapply(as.numeric(TempData$PsPCF12), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
# Work on File
TempData$RSD1 <- TempData$PsPCF1 # Just to size correctly (???)
TempData$RSD2 <- TempData$PsPCF1 
TempData$RSD3 <- TempData$PsPCF1 
TempData$RSD4 <- TempData$PsPCF1 
TempData$RSD5 <- TempData$PsPCF1 
TempData$RSD6 <- TempData$PsPCF1 
TempData$RSD7 <- TempData$PsPCF1 
TempData$RSD8 <- TempData$PsPCF1 
TempData$RSD9 <- TempData$PsPCF1 
TempData$RSD10 <- TempData$PsPCF1
TempData$RSD11 <- TempData$PsPCF1 
TempData$RSD12 <- TempData$PsPCF1 
TempData$RSD1 <- NA
TempData$RSD2 <- NA
TempData$RSD3 <- NA
TempData$RSD4 <- NA
TempData$RSD5 <- NA
TempData$RSD6 <- NA
TempData$RSD7 <- NA
TempData$RSD8 <- NA
TempData$RSD9 <- NA
TempData$RSD10 <- NA
TempData$RSD11 <- NA
TempData$RSD12 <- NA
TempData$AvRSD <- TempData$PsPCF1 # Size only
TempData$AvRSD <- NA
TempData$Arb1 <- TempData$PsPCF1 # Size only
TempData$Arb1 <- NA
LengthTempData <- length(TempData$PsPCF1)
for(i in RollingWindow:LengthTempData){
# Align rolling standard deviation vector with dataframe  
  k = i - RollingWindow + 1 
  TempData$RSD1[i] <- as.numeric(RSD1[k]) 
  TempData$RSD2[i] <- as.numeric(RSD2[k]) 
  TempData$RSD3[i] <- as.numeric(RSD3[k]) 
  TempData$RSD4[i] <- as.numeric(RSD4[k]) 
  TempData$RSD5[i] <- as.numeric(RSD5[k]) 
  TempData$RSD6[i] <- as.numeric(RSD6[k]) 
  TempData$RSD7[i] <- as.numeric(RSD7[k]) 
  TempData$RSD8[i] <- as.numeric(RSD8[k]) 
  TempData$RSD9[i] <- as.numeric(RSD9[k]) 
  TempData$RSD10[i] <- as.numeric(RSD10[k]) 
  TempData$RSD11[i] <- as.numeric(RSD11[k]) 
  TempData$RSD12[i] <- as.numeric(RSD12[k]) 
  # Work on Arbitrageness measures
  # 12 Month Version
  if(ArbMaturity == 12){
    TempData$AvRSD[i] <- (TempData$RSD1[i] + TempData$RSD2[i] + 
      TempData$RSD3[i] + TempData$RSD4[i] + TempData$RSD5[i] + 
      TempData$RSD6[i] + TempData$RSD7[i] + TempData$RSD8[i] + 
      TempData$RSD9[i] + TempData$RSD10[i] + TempData$RSD11[i] + 
      TempData$RSD12[i])/12.0
    TempData$Arb1[i] <-  TempData$RSD1[i]/TempData$AvRSD[i] - 
      TempData$RSD12[i]/TempData$AvRSD[i]
  } else if (ArbMaturity == 6){
    TempData$AvRSD[i] <- (TempData$RSD1[i] + TempData$RSD2[i] + 
        TempData$RSD3[i] + TempData$RSD4[i] + TempData$RSD5[i] + 
        TempData$RSD6[i])/6.0
    TempData$Arb1[i] <-  TempData$RSD1[i]/TempData$AvRSD[i] - 
      TempData$RSD6[i]/TempData$AvRSD[i]
  }
  # TempData$Arb1[i] <- -TempData$Arb1[i]
}
#
# Work on slope coefficient
#
TempLSC = data.frame(matrix(vector(), ArbMaturity, 7, dimnames=list(c(), 
  c("Y", "X1", "X2", "X3", "X4", "X5", "X6"
  ))), stringsAsFactors=F)
NFactors = 3 # Must be between 2 and 6
Tau1 = 2
Tau2 = 1
Tau3 = 0
Tau4 = 0
for (i in 1:ArbMaturity){
  Maturity = i
  if (NFactors == 1) {
    TempLSC$X1[i] = 1
    TempLSC$X2[i] = 0
    TempLSC$X3[i] = 0
    TempLSC$X4[i] = 0
    TempLSC$X5[i] = 0
    TempLSC$X6[i] = 0
  } else if (NFactors == 2){
    TempLSC$X1[i] = 1
    TempLSC$X2[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1))
    TempLSC$X3[i] = 0
    TempLSC$X4[i] = 0
    TempLSC$X5[i] = 0
    TempLSC$X6[i] = 0
  } else if (NFactors == 3){
    TempLSC$X1[i] = 1
    TempLSC$X2[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1))
    TempLSC$X3[i]=(Tau1/Maturity)*(1-exp(-Maturity/Tau1))-exp(-Maturity/Tau1)
    TempLSC$X4[i] = 0
    TempLSC$X5[i] = 0
    TempLSC$X6[i] = 0
  } else if (NFactors == 4){
    TempLSC$X1[i] = 1
    TempLSC$X2[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1))
    TempLSC$X3[i]=(Tau1/Maturity)*(1-exp(-Maturity/Tau1))-exp(-Maturity/Tau1)
    TempLSC$X4[i]=(Tau2/Maturity)*(1-exp(-Maturity/Tau2))-exp(-Maturity/Tau2)
    TempLSC$X5[i] = 0
    TempLSC$X6[i] = 0
  } else if (NFactors == 5){
    TempLSC$X1[i] = 1
    TempLSC$X2[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1))
    TempLSC$X3[i]=(Tau1/Maturity)*(1-exp(-Maturity/Tau1))-exp(-Maturity/Tau1)
    TempLSC$X4[i]=(Tau2/Maturity)*(1-exp(-Maturity/Tau2))-exp(-Maturity/Tau2)
    TempLSC$X5[i]=(Tau3/Maturity)*(1-exp(-Maturity/Tau3))-exp(-Maturity/Tau3)
    TempLSC$X6[i] = 0
  }  else if (NFactors == 6){
    TempLSC$X1[i] = 1
    TempLSC$X2[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1))
    TempLSC$X3[i]=(Tau1/Maturity)*(1-exp(-Maturity/Tau1))-exp(-Maturity/Tau1)
    TempLSC$X4[i]=(Tau2/Maturity)*(1-exp(-Maturity/Tau2))-exp(-Maturity/Tau2)
    TempLSC$X5[i]=(Tau3/Maturity)*(1-exp(-Maturity/Tau3))-exp(-Maturity/Tau3)
    TempLSC$X6[i]=(Tau4/Maturity)*(1-exp(-Maturity/Tau4))-exp(-Maturity/Tau4)
  }    
}
TempData$Intercept = TempData$PsPCF1 # Size only
TempData$Slope = TempData$PsPCF1 # Size only
TempData$Curv1 = TempData$PsPCF1 # Size only
TempData$Intercept = NA
TempData$Slope = NA
TempData$Curv1 = NA
RegTest = 99
for(i in RollingWindow:LengthTempData){
  if (ArbMaturity == 12){
    AvRSD <- (TempData$RSD1[i] + TempData$RSD2[i] + TempData$RSD3[i] +
      TempData$RSD4[i] + TempData$RSD5[i] + TempData$RSD6[i] + 
      TempData$RSD7[i] + TempData$RSD8[i] + TempData$RSD9[i] + 
      TempData$RSD10[i] + TempData$RSD11[i] + TempData$RSD12[i])/12.0  
    if(is.finite(TempData$RSD1[i])){TempLSC$Y[1] = TempData$RSD1[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD2[i])){TempLSC$Y[2] = TempData$RSD2[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD3[i])){TempLSC$Y[3] = TempData$RSD3[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD4[i])){TempLSC$Y[4] = TempData$RSD4[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD5[i])){TempLSC$Y[5] = TempData$RSD5[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD6[i])){TempLSC$Y[6] = TempData$RSD6[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD7[i])){TempLSC$Y[7] = TempData$RSD7[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD8[i])){TempLSC$Y[8] = TempData$RSD8[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD9[i])){TempLSC$Y[9] = TempData$RSD9[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD10[i])){TempLSC$Y[10] = TempData$RSD10[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD11[i])){TempLSC$Y[11] = TempData$RSD11[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD12[i])){TempLSC$Y[12] = TempData$RSD12[i]/AvRSD}
      else{RegTest = -99}
  } else if (ArbMaturity == 6){
    AvRSD <- (TempData$RSD1[i] + TempData$RSD2[i] + TempData$RSD3[i] +
      TempData$RSD4[i] + TempData$RSD5[i] + TempData$RSD6[i])/6.0  
    if(is.finite(TempData$RSD1[i])){TempLSC$Y[1] = TempData$RSD1[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD2[i])){TempLSC$Y[2] = TempData$RSD2[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD3[i])){TempLSC$Y[3] = TempData$RSD3[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD4[i])){TempLSC$Y[4] = TempData$RSD4[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD5[i])){TempLSC$Y[5] = TempData$RSD5[i]/AvRSD}
      else{RegTest = -99}
    if(is.finite(TempData$RSD6[i])){TempLSC$Y[6] = TempData$RSD6[i]/AvRSD}
      else{RegTest = -99}
  }
  if(RegTest > 0){
    if(NFactors == 3){
      LSC <- try(lm(formula = TempLSC$Y~TempLSC$X2+TempLSC$X3))
      if(is(LSC, "try-error")) {
        TempData$Intercept[i] = NA
        TempData$Slope[i] = NA
        TempData$Curv1[i] = NA
      } else {
        TempData$Intercept[i] = LSC$coefficients[1]
        TempData$Slope[i] = -LSC$coefficients[2]
        TempData$Curv1[i] = LSC$coefficients[3]
      }
    }
  } else {
    TempData$Intercept[i] = NA
    TempData$Slope[i] = NA
    TempData$Curv1[i] = NA
    RegTest = 99
  }
} 
TempData$Arb2 <- TempData$Slope
#
# Create monthly data set
#
TMonth <- as.numeric(TempData$Month)
DiffMonth <- diff(TMonth)
TempData$DMonth <- TempData$Arb2 # Just to size to correctly
TempData$DMonth <- 0
for(i in 2:LengthTempData){
  TempData$DMonth[i] <- DiffMonth[i-1]
}
Count <- 0
for(i in 2:LengthTempData){
  if(TempData$DMonth[i] != 0)Count = Count + 1
}
LengthMData <- Count
#
# Data frame for Monthly data
#
MData = data.frame(matrix(vector(), LengthMData, 12, dimnames=list(c(), 
  c("Date", "Date1", "Month", "Day", "Year", "Arb1",
    "Arb2", "Arb3", "Arb4", "Arb5", "Arb6", "Arb7"
  ))), stringsAsFactors=F)
k = 0
for(i in 1:LengthTempData){
  if(TempData$DMonth[i] != 0){
    k = k + 1
    MData$Date[k] <- TempData$Date[i]
    MData$Date1[k] <- TempData$Date1[i]
    MData$Month[k] <- TempData$Month[i]
    MData$Day[k] <- TempData$Day[i]
    MData$Year[k] <- TempData$Year[i]
    MData$Arb1[k] <- TempData$Arb1[i]
    MData$Arb2[k] <- TempData$Arb2[i]
    MData$Arb3[k] <- TempData$Arb3[i]
    MData$Arb4[k] <- TempData$Arb4[i]
    MData$Arb5[k] <- TempData$Arb5[i]
    MData$Arb6[k] <- TempData$Arb6[i]
    MData$Arb7[k] <- TempData$Arb7[i]
  }
}
TempDataz <- zoo(TempData, TempData$Date)
MData$Date <- as.Date(mdy.date(month = MData$Month, day = MData$Day, 
  year = MData$Year))    
MDataz <- zoo(MData, MData$Date)
# Arb1
MDataAverage1 <- mean(MData$Arb1, na.rm = TRUE)
MDataStdDev1 <- as.numeric(StdDev(MData$Arb1, na.rm = TRUE))
MDataz$Average1 <- MDataz$Arb1 # Size
MDataz$Average1 <- MDataAverage1
MDataz$StdDev1 <- MDataz$Arb1 # Size
MDataz$StdDev1 <- MDataStdDev1
# Arb2
MDataAverage2 <- mean(MData$Arb2, na.rm = TRUE)
MDataStdDev2 <- as.numeric(StdDev(MData$Arb2, na.rm = TRUE))
MDataz$Average2 <- MDataz$Arb2 # Size
MDataz$Average2 <- MDataAverage2
MDataz$StdDev2 <- MDataz$Arb2 # Size
MDataz$StdDev2 <- MDataStdDev2
# Arb3
MDataAverage3 <- mean(MData$Arb3, na.rm = TRUE)
MDataStdDev3 <- as.numeric(StdDev(MData$Arb3, na.rm = TRUE))
MDataz$Average3 <- MDataz$Arb3 # Size
MDataz$Average3 <- MDataAverage3
MDataz$StdDev3 <- MDataz$Arb3 # Size
MDataz$StdDev3 <- MDataStdDev3
# Arb4
MDataAverage4 <- mean(MData$Arb4, na.rm = TRUE)
MDataStdDev4 <- as.numeric(StdDev(MData$Arb4, na.rm = TRUE))
MDataz$Average4 <- MDataz$Arb4 # Size
MDataz$Average4 <- MDataAverage4
MDataz$StdDev4 <- MDataz$Arb4 # Size
MDataz$StdDev4 <- MDataStdDev4
# Arb5
MDataAverage5 <- mean(MData$Arb5, na.rm = TRUE)
MDataStdDev5 <- as.numeric(StdDev(MData$Arb5, na.rm = TRUE))
MDataz$Average5 <- MDataz$Arb5 # Size
MDataz$Average5 <- MDataAverage5
MDataz$StdDev5 <- MDataz$Arb5 # Size
MDataz$StdDev5 <- MDataStdDev5
# Arb6
MDataAverage6 <- mean(MData$Arb6, na.rm = TRUE)
MDataStdDev6 <- as.numeric(StdDev(MData$Arb6, na.rm = TRUE))
MDataz$Average6 <- MDataz$Arb6 # Size
MDataz$Average6 <- MDataAverage6
MDataz$StdDev6 <- MDataz$Arb6 # Size
MDataz$StdDev6 <- MDataStdDev6
# Arb7
MDataAverage7 <- mean(MData$Arb7, na.rm = TRUE)
MDataStdDev7 <- as.numeric(StdDev(MData$Arb7, na.rm = TRUE))
MDataz$Average7 <- MDataz$Arb7 # Size
MDataz$Average7 <- MDataAverage7
MDataz$StdDev7 <- MDataz$Arb7 # Size
MDataz$StdDev7 <- MDataStdDev7
#
# Preliminary Plots
#
x <- TempData$Date1
xI <- as.Date(TempData$Date)
MaxValueX = max(x)
MinValueX = min(x)
xlim1 = c(1:2)
xlim1[1] = MinValueX
xlim1[2] = MaxValueX
IncrementX = (as.numeric(MaxValueX) - as.numeric(MinValueX))/7.0
TickMarksX = c(seq(from=MinValueX,to=MaxValueX,by=IncrementX)) # Numeric
TickMarksX <- as.integer(round(TickMarksX,0)) # Integer
# Julian numbers based on MS Excel
TickMarksXI <- as.Date(TickMarksX, origin=as.Date("1900-01-01"))
TickMarksXI <- format(TickMarksXI, format = "%Y-%m")
lblX = format(TickMarksXI, format = "%Y-%m")
# Preliminary analysis
ArbPlot <- 2
y1 <- round((TempData$RSD1*100)*sqrt(252),2)
y2 <- round((TempData$RSD6*100)*sqrt(252),2)
legtxt = c("Pseudo Nearby", "Pseudo Sixth Nearby")
lTitle = "Rolling Standard Deviations"
xTitle = "Date"
yTitle = "Annualized Standard Deviations (%)"
source('Arb Plots.R')
# Preliminary analysis
ArbPlot <- 2
y1 <- (TempData$RSD2*100)*sqrt(252) -
  (TempData$RSD4*100)*sqrt(252)
y2 <- (TempData$RSD4*100)*sqrt(252) -
  (TempData$RSD6*100)*sqrt(252)
legtxt = c("Pseudo 2 - 4", "Pseudo 4 - 6")
lTitle = "Rolling Standard Deviation Differences"
xTitle = "Date"
yTitle = "Annualized Standard Deviations (%)"
source('Arb Plots.R')
# Daily slope
ArbPlot <- 1
y1 <- TempData$Slope
legtxt = c("Slope")
lTitle = "LSC Parameters"
xTitle = "Date"
yTitle = "Slope"
source('Arb Plots.R')
# Daily slope and curvature
ArbPlot <- 2
y1 <- TempData$Slope
y2 <- TempData$Curv1
legtxt = c("Slope", "Curv1")
lTitle = "LSC Parameters"
xTitle = "Date"
yTitle = "LSC Parameters"
source('Arb Plots.R')
# Daily slope and percentage change slope (SAS-based ???)
ArbPlot <- 2
y1 <- TempData$Slope
y2 <- TempData$Arb3
legtxt = c("SD Slope", "PC Slope")
lTitle = "LSC Parameters"
xTitle = "Date"
yTitle = "LSC Parameters"
source('Arb Plots.R')
# Pseudo-Term Premium 2 and 3
ArbPlot <- 2
y1 <- TempData$Arb4
y2 <- TempData$Arb5
legtxt = c("Term Premium 2", "Term Premium 3")
lTitle = "Parameters"
xTitle = "Date"
yTitle = "Term Premium"
source('Arb Plots.R')
# Pseudo-Marginal Contribution 2 and 3
ArbPlot <- 2
y1 <- TempData$Arb6
y2 <- TempData$Arb7
legtxt = c("Marginal Contribution 2", "Marginal Contribution 3")
lTitle = "Parameters"
xTitle = "Date"
yTitle = "Marginal Contribution"
source('Arb Plots.R')
#
# Monthly Plots (First Trading Day of Month)
#
#display.brewer.all() # All palette available from RColorBrewer
cols<-brewer.pal(n=9,name="Paired") # First 9 colors in Paired palette (prints well)
#cols #cols contain the names of four different colors
x <- index(MDataz)
MaxValueX = max(x)
MinValueX = min(x)
xlim1 = c(1:2)
xlim1[1] = MinValueX
xlim1[2] = MaxValueX
IncrementX = (as.numeric(MaxValueX) - as.numeric(MinValueX))/7.0
TickMarksX = c(seq(from=MinValueX,to=MaxValueX,by=IncrementX))
lblX = format.Date(TickMarksX, "%Y-%m")
# Arb1: Nearby - nth Nearby (6 or 12) ratio of RSD to Average RSD (Normalized)
y1 <- as.numeric(MDataz$Arb1)
y2 <- as.numeric(MDataz$Average1)
legtxt = c("Arb1", "Average")
lTitle = "Difference in Ratio of RSD to Average RSD (1 - 6)"
xTitle = "Date"
yTitle = "Difference"
source('Arb Plots.R')
# Arb2: LSC slope of normalized RSD
y1 <- as.numeric(MDataz$Arb2)
y2 <- as.numeric(MDataz$Average2)
legtxt = c("Slope", "Average")
lTitle = "LSC Slope of Normalized RSD"
xTitle = "Date"
yTitle = "Slope"
source('Arb Plots.R')
# Arb3: Slope of percentage change LSC
y1 <- as.numeric(MDataz$Arb3)
y2 <- as.numeric(MDataz$Average3)
legtxt = c("Slope", "Average")
lTitle = "Percentage Change LSC"
xTitle = "Date"
yTitle = "Slope"
source('Arb Plots.R')
# Arb4: Term premium 2
y1 <- as.numeric(MDataz$Arb4)
y2 <- as.numeric(MDataz$Average4)
legtxt = c("Second Nearby", "Average")
lTitle = "Term Premium"
xTitle = "Date"
yTitle = "Term Premium"
source('Arb Plots.R')
# Arb5: Term premium 3
y1 <- as.numeric(MDataz$Arb5)
y2 <- as.numeric(MDataz$Average5)
legtxt = c("Third Nearby", "Average")
lTitle = "Term Premium"
xTitle = "Date"
yTitle = "Term Premium"
source('Arb Plots.R')
# Arb6: Marginal contribution 2
y1 <- as.numeric(MDataz$Arb6)
y2 <- as.numeric(MDataz$Average6)
legtxt = c("Second Nearby", "Average")
lTitle = "Marginal Contribution"
xTitle = "Date"
yTitle = "Marginal Contribution"
source('Arb Plots.R')
# Arb7: Marginal contribution 3
y1 <- as.numeric(MDataz$Arb7)
y2 <- as.numeric(MDataz$Average7)
legtxt = c("Third Nearby", "Average")
lTitle = "Marginal Contribution"
xTitle = "Date"
yTitle = "Marginal Contribution"
source('Arb Plots.R')
