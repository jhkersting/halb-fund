# Instrument 2.R
FP <- read.xlsx(FileName, sheet = 1, startRow = 1, colNames = TRUE,
  rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
  rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
FP <- FP[FP$Year >= StartYear,]
FP <- FP[FP$Year <= EndYear,]
#
# Focus just on spot and spot Returns (percentage change, continuously compounded)
#
if(Nearby == 0){
  Price <- FP$CSpot  
} else if(Nearby == 1){
  Price <- FP$C1
} else if(Nearby == 2){
  Price <- FP$C2
} else if(Nearby == 3){
  Price <- FP$C3
} else if(Nearby == 4){
  Price <- FP$C4
} else if(Nearby == 5){
  Price <- FP$C5
} else if(Nearby == 6){
  Price <- FP$C6
}
# Price <- FP$C3 #FP$CSpot # FP$C?
# Return <- FP$PCF2 #FP$PCSpot # FP$PCF?
Month <- zoo(FP$Month)
Day <- zoo(FP$Day)
Year <- zoo(FP$Year)
rm(FP, FileName)
# Month, Day, Year, Price, Return
CDate = mdy.date(Month, Day, Year)
ZDate <- zoo(CDate)
ZPrice <- zoo(Price)
LagPrice <- lag(ZPrice, -1, na.pad = TRUE)
CCReturn <- log(ZPrice) - log(LagPrice) # Continuously compounded rate of Return
DCReturn <- (ZPrice - LagPrice)/LagPrice # Discretely compounded rate of Return
ACPrice <- ZPrice - LagPrice # Actual change in Price (ABM measure)
LengthPrice = length(ZPrice)
dfStockRisk2 = data.frame(matrix(vector(), LengthPrice, 22, dimnames=list(c(), 
  c("Date", "Month", "Day", "Year", "Price", "CCReturn", "DCReturn", 
    "ACPrice", "CCStDev", "DCStDev", "ACStDev", "ACCCStDev", "ACDCStDev",
    "CCMean", "DCMean", "ACMean","CCSkewness", "DCSkewness", "ACSkewness",
    "CCKurtosis", "DCKurtosis", "ACKurtosis"
    ))), stringsAsFactors=F)
dfStockRisk2$Date <- ZDate
dfStockRisk2$Month <- Month
dfStockRisk2$Day <- Day
dfStockRisk2$Year <- Year
dfStockRisk2$Price <- ZPrice
dfStockRisk2$CCReturn <- CCReturn
dfStockRisk2$DCReturn <- DCReturn
dfStockRisk2$ACPrice <- ACPrice
dfStockRisk2 <- dfStockRisk2[-1,]      # Remove first line (NAs)
CCMean <- rollapply(dfStockRisk2$CCReturn, RollingWindow, 
  function(x)(mean(x)), align = "right")
DCMean <- rollapply(dfStockRisk2$DCReturn, RollingWindow, 
  function(x)(mean(x)), align = "right")
ACMean <- rollapply(dfStockRisk2$ACPrice, RollingWindow, 
  function(x)(mean(x)), align = "right")
CCStDev <- rollapply(dfStockRisk2$CCReturn, RollingWindow, 
  function(x)(sqrt(var(x))), align = "right")
DCStDev <- rollapply(dfStockRisk2$DCReturn, RollingWindow, 
  function(x)(sqrt(var(x))), align = "right")
ACStDev <- rollapply(dfStockRisk2$ACPrice, RollingWindow, 
  function(x)(sqrt(var(x))), align = "right")
CCSkewness <- rollapply(dfStockRisk2$CCReturn, RollingWindow, 
  function(x)(skewness(x)), align = "right")
DCSkewness <- rollapply(dfStockRisk2$DCReturn, RollingWindow, 
  function(x)(skewness(x)), align = "right")
ACSkewness <- rollapply(dfStockRisk2$ACPrice, RollingWindow, 
  function(x)(skewness(x)), align = "right")
CCKurtosis <- rollapply(dfStockRisk2$CCReturn, RollingWindow, 
  function(x)(kurtosis(x)), align = "right")
DCKurtosis <- rollapply(dfStockRisk2$DCReturn, RollingWindow, 
  function(x)(kurtosis(x)), align = "right")
ACKurtosis <- rollapply(dfStockRisk2$ACPrice, RollingWindow, 
  function(x)(kurtosis(x)), align = "right")
j = RollingWindow + 1
LPM1 = LengthPrice - 1
for(i in j:LPM1){
  k = i-j+2 # Align rolling standard deviation vector with dataframe
  dfStockRisk2$CCMean[i] <- CCMean[k]
  dfStockRisk2$DCMean[i] <- DCMean[k]
  dfStockRisk2$ACMean[i] <- ACMean[k]
  dfStockRisk2$CCStDev[i] <- CCStDev[k]
  dfStockRisk2$DCStDev[i] <- DCStDev[k]
  dfStockRisk2$ACStDev[i] <- ACStDev[k]
  dfStockRisk2$CCSkewness[i] <- CCSkewness[k]
  dfStockRisk2$DCSkewness[i] <- DCSkewness[k]
  dfStockRisk2$ACSkewness[i] <- ACSkewness[k]
  dfStockRisk2$CCKurtosis[i] <- CCKurtosis[k]
  dfStockRisk2$DCKurtosis[i] <- DCKurtosis[k]
  dfStockRisk2$ACKurtosis[i] <- ACKurtosis[k]
# Estimates of dollar change << OLD LIKELY DELETE >>
  dfStockRisk2$ACCCStDev[i] <- CCStDev[k]*dfStockRisk2$Price[i]
  dfStockRisk2$ACDCStDev[i] <- DCStDev[k]*dfStockRisk2$Price[i]
}
k = 0
NSD = LPM1 - RollingWindow
X <- as.date(c(1:NSD))
X2 <- as.double(c(1:NSD))
Y1 <- as.double(c(1:NSD))
Y2 <- as.double(c(1:NSD))
Y3 <- as.double(c(1:NSD))
Y4 <- as.double(c(1:NSD))
Y5 <- as.double(c(1:NSD))
Y6 <- as.double(c(1:NSD))
Y7 <- as.double(c(1:NSD))
Y8 <- as.double(c(1:NSD))
Y9 <- as.double(c(1:NSD))
Y10 <- as.double(c(1:NSD))
Y11 <- as.double(c(1:NSD))
Y12 <- as.double(c(1:NSD))
Y13 <- as.double(c(1:NSD))
Y14 <- as.double(c(1:NSD))
Y15 <- as.double(c(1:NSD))
Y16 <- as.double(c(1:NSD))
Y17 <- as.double(c(1:NSD))
for(i in 1:LPM1){
  if(!is.na(dfStockRisk2$ACCCStDev[i])){
    k = k + 1
    X[k] <- dfStockRisk2$Date[i]
    Y1[k] <- dfStockRisk2$ACCCStDev[i]
    Y2[k] <- dfStockRisk2$ACDCStDev[i]
    Y3[k] <- dfStockRisk2$ACStDev[i]
    Y4[k] <- dfStockRisk2$ACStDev[i] - dfStockRisk2$ACCCStDev[i]
    Y5[k] <- dfStockRisk2$ACStDev[i] - dfStockRisk2$ACDCStDev[i]
    X2[k] <- dfStockRisk2$Price[i]
    Y6[k] <- dfStockRisk2$CCMean[i]
    Y7[k] <- dfStockRisk2$DCMean[i]
    Y8[k] <- dfStockRisk2$ACMean[i]/dfStockRisk2$Price[i] # Normalized
    Y9[k] <- dfStockRisk2$CCStDev[i]
    Y10[k] <- dfStockRisk2$DCStDev[i]
    Y11[k] <- dfStockRisk2$ACStDev[i]/dfStockRisk2$Price[i] # Normalized
    Y12[k] <- dfStockRisk2$CCSkewness[i]
    Y13[k] <- dfStockRisk2$DCSkewness[i]
    Y14[k] <- dfStockRisk2$ACSkewness[i]
    Y15[k] <- dfStockRisk2$CCKurtosis[i]
    Y16[k] <- dfStockRisk2$DCKurtosis[i]
    Y17[k] <- dfStockRisk2$ACKurtosis[i]
  }
}
#
# Underlying 1
#
MaxValue = max(X); MinValue = min(X)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
MaxValue = max(X2); MinValue = min(X2)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
xTitle = "Date"
yTitle = "Price"
plot(X, X2, type = "p", main = mTitle2,
  xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.25)
#
# Mean
#
MaxValue = max(X); MinValue = min(X)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
MaxValue = max(Y6, Y7, Y8); MinValue = min(Y6, Y7, Y8)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
xTitle = "Date"
yTitle = "Mean"
legtxt = c("CC Return","DC Return","Dollar Change")
lTitle = "Parameter"
plot(X, Y6, type = "p", main = mTitle2,
  xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y7, type = "p", col ="red", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
lines(X, Y8, type = "p", col ="green", xlim = xlim1, ylim = ylim1,
  pch = 3, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("blue","red","green"), pch = c(1,2,3), bty = "n", title = lTitle)
#
# Standard Deviations
#
MaxValue = max(X); MinValue = min(X)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
MaxValue = max(Y9, Y10, Y11); MinValue = min(Y9, Y10, Y11)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
xTitle = "Date"
yTitle = "Standard Deviation"
legtxt = c("CC Return","DC Return","Dollar Change")
lTitle = "Parameter"
plot(X, Y9, type = "b", main = mTitle2,
  xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y10, type = "b", col ="red", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
lines(X, Y11, type = "b", col ="green", xlim = xlim1, ylim = ylim1,
  pch = 3, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1), 
  col = c("blue","red","green"), pch = c(1,2,3), bty = "n", title = lTitle)
#
# Skewness
#
MaxValue = max(X); MinValue = min(X)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
MaxValue = max(Y12, Y13, Y14); MinValue = min(Y12, Y13, Y14)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
xTitle = "Date"
yTitle = "Skewness"
legtxt = c("CC Return","DC Return","Dollar Change")
lTitle = "Parameter"
plot(X, Y12, type = "b", main = mTitle2,
  xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y13, type = "b", col ="red", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
lines(X, Y14, type = "b", col ="green", xlim = xlim1, ylim = ylim1,
  pch = 3, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1), 
  col = c("blue","red","green"), pch = c(1,2,3), bty = "n", title = lTitle)
#
# Kurtosis
#
MaxValue = max(X); MinValue = min(X)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
MaxValue = max(Y15, Y16, Y17); MinValue = min(Y15, Y16, Y17)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
xTitle = "Date"
yTitle = "Kurtosis"
legtxt = c("CC Return","DC Return","Dollar Change")
lTitle = "Parameter"
plot(X, Y15, type = "b", main = mTitle2,
  xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y16, type = "b", col ="red", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
lines(X, Y17, type = "b", col ="green", xlim = xlim1, ylim = ylim1,
  pch = 3, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("blue","red","green"), pch = c(1,2,3), bty = "n", title = lTitle)
