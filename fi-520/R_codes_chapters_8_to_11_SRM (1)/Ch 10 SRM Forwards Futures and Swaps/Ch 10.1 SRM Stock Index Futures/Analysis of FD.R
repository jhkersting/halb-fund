# Analysis of FD.R 
#
# Analysis of percentage change in futures prices
#
rm(TempData)
FileLocation <- paste(File, sep = "")
Temp <- read.xlsx(FileLocation)
Temp <- Temp[Temp$Date <= EndDate,]
Temp <- Temp[Temp$Date >= StartDate,]
FileLength <- length(Temp$Date)
TempData = data.frame(matrix(vector(), FileLength, 22, dimnames=list(c(), 
  c("Date", "Date1", "Month", "Day", "Year",
    "PsFD1", "PsFD2", "PsFD3", "PsFD4", "PsFD5", "PsFD6", "PsFD7", 
    "PsFD8", "PsFD9", "PsFD10", "PsFD11", "PsFD12",
    "PsTP2", "PsTP3", "PsMC2", "PsMC3", "FDSlope"
  ))), stringsAsFactors=F)
TempData$Date <- Temp$Date
TempData$Date1 <- Temp$Date1
TempData$Month <- Temp$Month
TempData$Day <- Temp$Day
TempData$Year <- Temp$Year
TempData$Date <- as.Date(mdy.date(month = TempData$Month, day = TempData$Day, 
  year = TempData$Year))
TempData$PsFD1 <- append(diff(Temp$PsC1, lag=1, differences=1), NA, after=0) 
TempData$PsFD2 <- append(diff(Temp$PsC2, lag=1, differences=1), NA, after=0) 
TempData$PsFD3 <- append(diff(Temp$PsC3, lag=1, differences=1), NA, after=0) 
TempData$PsFD4 <- append(diff(Temp$PsC4, lag=1, differences=1), NA, after=0) 
TempData$PsFD5 <- append(diff(Temp$PsC5, lag=1, differences=1), NA, after=0) 
TempData$PsFD6 <- append(diff(Temp$PsC6, lag=1, differences=1), NA, after=0) 
TempData$PsFD7 <- append(diff(Temp$PsC7, lag=1, differences=1), NA, after=0) 
TempData$PsFD8 <- append(diff(Temp$PsC8, lag=1, differences=1), NA, after=0) 
TempData$PsFD9 <- append(diff(Temp$PsC9, lag=1, differences=1), NA, after=0) 
TempData$PsFD10 <- append(diff(Temp$PsC10, lag=1, differences=1), NA, after=0) 
TempData$PsFD11 <- append(diff(Temp$PsC11, lag=1, differences=1), NA, after=0) 
TempData$PsFD12 <- append(diff(Temp$PsC12, lag=1, differences=1), NA, after=0) 

TempData$Arb3 <- Temp$FDSlope
TempData$Arb4 <- Temp$PsTP2
TempData$Arb5 <- Temp$PsTP3
TempData$Arb6 <- Temp$PsMC2
TempData$Arb7 <- Temp$PsMC3
#rm(Temp) 
# Rolling weighted standard deviations
RSD1 <- rollapply(as.numeric(TempData$PsFD1), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD2 <- rollapply(as.numeric(TempData$PsFD2), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD3 <- rollapply(as.numeric(TempData$PsFD3), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD4 <- rollapply(as.numeric(TempData$PsFD4), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD5 <- rollapply(as.numeric(TempData$PsFD5), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD6 <- rollapply(as.numeric(TempData$PsFD6), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD7 <- rollapply(as.numeric(TempData$PsFD7), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD8 <- rollapply(as.numeric(TempData$PsFD8), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD9 <- rollapply(as.numeric(TempData$PsFD9), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD10 <- rollapply(as.numeric(TempData$PsFD10), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD11 <- rollapply(as.numeric(TempData$PsFD11), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
RSD12 <- rollapply(as.numeric(TempData$PsFD12), RollingWindow, 
  function(x, wt)(sqrt(wtd.var(x, wt))), Weights*100, align = "right")
# Work on File
TempData$RSD1 <- TempData$PsFD1 # Just to size correctly (???)
TempData$RSD2 <- TempData$PsFD1 
TempData$RSD3 <- TempData$PsFD1 
TempData$RSD4 <- TempData$PsFD1 
TempData$RSD5 <- TempData$PsFD1 
TempData$RSD6 <- TempData$PsFD1 
TempData$RSD7 <- TempData$PsFD1 
TempData$RSD8 <- TempData$PsFD1 
TempData$RSD9 <- TempData$PsFD1 
TempData$RSD10 <- TempData$PsFD1
TempData$RSD11 <- TempData$PsFD1 
TempData$RSD12 <- TempData$PsFD1 
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
TempData$AvRSD <- TempData$PsFD1 
TempData$AvRSD <- NA
TempData$Arb1 <- TempData$PsFD1 
TempData$Arb1 <- NA
LengthTempData <- length(TempData$PsFD1)
for(i in RollingWindow:LengthTempData){
  k = i - RollingWindow + 1 # Align rolling standard deviation vector with dataframe
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
    TempData$AvRSD[i] <- (TempData$RSD1[i] + TempData$RSD2[i] + TempData$RSD3[i] +
        TempData$RSD4[i] + TempData$RSD5[i] + TempData$RSD6[i] + TempData$RSD7[i] + 
        TempData$RSD8[i] + TempData$RSD9[i] + TempData$RSD10[i] + TempData$RSD11[i] + 
        TempData$RSD12[i])/12.0
    TempData$Arb1[i] <-  TempData$RSD1[i]/TempData$AvRSD[i] - TempData$RSD12[i]/TempData$AvRSD[i]
  } else if (ArbMaturity == 6){
    TempData$AvRSD[i] <- (TempData$RSD1[i] + TempData$RSD2[i] + TempData$RSD3[i] +
        TempData$RSD4[i] + TempData$RSD5[i] + TempData$RSD6[i])/6.0
    TempData$Arb1[i] <-  TempData$RSD1[i]/TempData$AvRSD[i] - TempData$RSD6[i]/TempData$AvRSD[i]
  }
  TempData$Arb1[i] <- -TempData$Arb1[i]
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
    TempLSC$X3[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1)) - exp(-Maturity/Tau1)
    TempLSC$X4[i] = 0
    TempLSC$X5[i] = 0
    TempLSC$X6[i] = 0
  } else if (NFactors == 4){
    TempLSC$X1[i] = 1
    TempLSC$X2[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1))
    TempLSC$X3[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1)) - exp(-Maturity/Tau1)
    TempLSC$X4[i] = (Tau2/Maturity)*(1-exp(-Maturity/Tau2)) - exp(-Maturity/Tau2)
    TempLSC$X5[i] = 0
    TempLSC$X6[i] = 0
  } else if (NFactors == 5){
    TempLSC$X1[i] = 1
    TempLSC$X2[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1))
    TempLSC$X3[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1)) - exp(-Maturity/Tau1)
    TempLSC$X4[i] = (Tau2/Maturity)*(1-exp(-Maturity/Tau2)) - exp(-Maturity/Tau2)
    TempLSC$X5[i] = (Tau3/Maturity)*(1-exp(-Maturity/Tau3)) - exp(-Maturity/Tau3)
    TempLSC$X6[i] = 0
  }  else if (NFactors == 6){
    TempLSC$X1[i] = 1
    TempLSC$X2[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1))
    TempLSC$X3[i] = (Tau1/Maturity)*(1-exp(-Maturity/Tau1)) - exp(-Maturity/Tau1)
    TempLSC$X4[i] = (Tau2/Maturity)*(1-exp(-Maturity/Tau2)) - exp(-Maturity/Tau2)
    TempLSC$X5[i] = (Tau3/Maturity)*(1-exp(-Maturity/Tau3)) - exp(-Maturity/Tau3)
    TempLSC$X6[i] = (Tau4/Maturity)*(1-exp(-Maturity/Tau4)) - exp(-Maturity/Tau4)
  }    
}
TempData$Slope = TempData$PsFD1 # Size only
TempData$Slope = NA
RegTest = 99
for(i in RollingWindow:LengthTempData){
  if (ArbMaturity == 12){
    AvRSD <- (TempData$RSD1[i] + TempData$RSD2[i] + TempData$RSD3[i] +
        TempData$RSD4[i] + TempData$RSD5[i] + TempData$RSD6[i] + TempData$RSD7[i] + 
        TempData$RSD8[i] + TempData$RSD9[i] + TempData$RSD10[i] + TempData$RSD11[i] + 
        TempData$RSD12[i])/12.0  
    if(is.finite(TempData$RSD1[i])){TempLSC$Y[1] = TempData$RSD1[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD2[i])){TempLSC$Y[2] = TempData$RSD2[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD3[i])){TempLSC$Y[3] = TempData$RSD3[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD4[i])){TempLSC$Y[4] = TempData$RSD4[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD5[i])){TempLSC$Y[5] = TempData$RSD5[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD6[i])){TempLSC$Y[6] = TempData$RSD6[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD7[i])){TempLSC$Y[7] = TempData$RSD7[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD8[i])){TempLSC$Y[8] = TempData$RSD8[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD9[i])){TempLSC$Y[9] = TempData$RSD9[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD10[i])){TempLSC$Y[10] = TempData$RSD10[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD11[i])){TempLSC$Y[11] = TempData$RSD11[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD12[i])){TempLSC$Y[12] = TempData$RSD12[i]/AvRSD}else{RegTest = -99}
  } else if (ArbMaturity == 6){
    AvRSD <- (TempData$RSD1[i] + TempData$RSD2[i] + TempData$RSD3[i] +
        TempData$RSD4[i] + TempData$RSD5[i] + TempData$RSD6[i])/6.0  
    if(is.finite(TempData$RSD1[i])){TempLSC$Y[1] = TempData$RSD1[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD2[i])){TempLSC$Y[2] = TempData$RSD2[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD3[i])){TempLSC$Y[3] = TempData$RSD3[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD4[i])){TempLSC$Y[4] = TempData$RSD4[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD5[i])){TempLSC$Y[5] = TempData$RSD5[i]/AvRSD}else{RegTest = -99}
    if(is.finite(TempData$RSD6[i])){TempLSC$Y[6] = TempData$RSD6[i]/AvRSD}else{RegTest = -99}
  }
  if(RegTest > 0){
    if(NFactors == 3){
      LSC <- try(lm(formula = TempLSC$Y~TempLSC$X2+TempLSC$X3))
      if(is(LSC, "try-error")) {
        TempData$Slope[i] = NA
      } else {
        TempData$Slope[i] = -LSC$coefficients[2]
      }
    }
  } else {
    TempData$Slope[i] = NA
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
# head(MDataz, 5)
# 
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
lTitle = "Difference Measures: Ratio of RSD to Average RSD"
xTitle = "Date"
yTitle = "Difference"
source('Arb Plots.R')
# Arb2: LSC slope of normalized RSD
y1 <- as.numeric(MDataz$Arb2)
y2 <- as.numeric(MDataz$Average2)
legtxt = c("Arb2", "Average")
lTitle = "LSC Slope of Normalized RSD"
xTitle = "Date"
yTitle = "Slope"
source('Arb Plots.R')
# Arb3: Slope of percentage change LSC
y1 <- as.numeric(MDataz$Arb3)
y2 <- as.numeric(MDataz$Average3)
legtxt = c("Arb3", "Average")
lTitle = "Percentage Change LSC Slope"
xTitle = "Date"
yTitle = "Slope"
source('Arb Plots.R')
# Arb4: Term premium 2
y1 <- as.numeric(MDataz$Arb4)
y2 <- as.numeric(MDataz$Average4)
legtxt = c("Arb4", "Average")
lTitle = "Term Premium 2"
xTitle = "Date"
yTitle = "Percent"
source('Arb Plots.R')
# Arb5: Term premium 3
y1 <- as.numeric(MDataz$Arb5)
y2 <- as.numeric(MDataz$Average5)
legtxt = c("Arb5", "Average")
lTitle = "Term Premium 3"
xTitle = "Date"
yTitle = "Percent"
source('Arb Plots.R')
# Arb6: Marginal contribution 2
y1 <- as.numeric(MDataz$Arb6)
y2 <- as.numeric(MDataz$Average6)
legtxt = c("Arb6", "Average")
lTitle = "Marginal Contribution 2"
xTitle = "Date"
yTitle = "Percent"
source('Arb Plots.R')
# Arb7: Marginal contribution 3
y1 <- as.numeric(MDataz$Arb7)
y2 <- as.numeric(MDataz$Average7)
legtxt = c("Arb7", "Average")
lTitle = "Marginal Contribution 3"
xTitle = "Date"
yTitle = "Percent"
source('Arb Plots.R')
