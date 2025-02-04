# Instrument 1.R
FP <- read.csv(FileName)
FP <- FP[FP$Date >= StartDate,]
FP <- FP[FP$Date <= EndDate,]
FP$Year <- floor(FP$Date/10000)
FP$Month <- floor(FP$Date/100) - FP$Year*100
FP$Day <- FP$Date - FP$Year*10000 - FP$Month*100
Month <- zoo(FP$Month)
Day <- zoo(FP$Day)
Year <- zoo(FP$Year)
Price <- FP$Close
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
dfStockRisk1 = data.frame(matrix(vector(), LengthPrice, 22, dimnames=list(c(), 
  c("Date", "Month", "Day", "Year", "Price", "CCReturn", "DCReturn", 
    "ACPrice", "CCStDev", "DCStDev", "ACStDev", "ACCCStDev", "ACDCStDev",
    "CCMean", "DCMean", "ACMean","CCSkewness", "DCSkewness", "ACSkewness",
    "CCKurtosis", "DCKurtosis", "ACKurtosis"
    ))), stringsAsFactors=F)
dfStockRisk1$Date <- ZDate
dfStockRisk1$Month <- Month
dfStockRisk1$Day <- Day
dfStockRisk1$Year <- Year
dfStockRisk1$Price <- ZPrice
dfStockRisk1$CCReturn <- CCReturn
dfStockRisk1$DCReturn <- DCReturn
dfStockRisk1$ACPrice <- ACPrice
dfStockRisk1 <- dfStockRisk1[-1,]      # Remove first line (NAs)
CCMean <- rollapply(dfStockRisk1$CCReturn, RollingWindow, 
  function(x)(mean(x)), align = "right")
CCStDev <- rollapply(dfStockRisk1$CCReturn, RollingWindow, 
  function(x)(sqrt(var(x))), align = "right")
CCSkewness <- rollapply(dfStockRisk1$CCReturn, RollingWindow, 
  function(x)(skewness(x)), align = "right")
CCKurtosis <- rollapply(dfStockRisk1$CCReturn, RollingWindow, 
  function(x)(kurtosis(x)), align = "right")
DCMean <- rollapply(dfStockRisk1$DCReturn, RollingWindow, 
  function(x)(mean(x)), align = "right")
DCStDev <- rollapply(dfStockRisk1$DCReturn, RollingWindow, 
  function(x)(sqrt(var(x))), align = "right")
DCSkewness <- rollapply(dfStockRisk1$DCReturn, RollingWindow, 
  function(x)(skewness(x)), align = "right")
DCKurtosis <- rollapply(dfStockRisk1$DCReturn, RollingWindow, 
  function(x)(kurtosis(x)), align = "right")
ACMean <- rollapply(dfStockRisk1$ACPrice, RollingWindow, 
  function(x)(mean(x)), align = "right")
ACStDev <- rollapply(dfStockRisk1$ACPrice, RollingWindow, 
  function(x)(sqrt(var(x))), align = "right")
ACSkewness <- rollapply(dfStockRisk1$ACPrice, RollingWindow, 
  function(x)(skewness(x)), align = "right")
ACKurtosis <- rollapply(dfStockRisk1$ACPrice, RollingWindow, 
  function(x)(kurtosis(x)), align = "right")
j = RollingWindow + 1
LPM1 = LengthPrice - 1
for(i in j:LPM1){
  k = i-j+2 # Align rolling standard deviation vector with dataframe
  dfStockRisk1$CCMean[i] <- CCMean[k]
  dfStockRisk1$DCMean[i] <- DCMean[k]
  dfStockRisk1$ACMean[i] <- ACMean[k]
  dfStockRisk1$CCStDev[i] <- CCStDev[k]
  dfStockRisk1$DCStDev[i] <- DCStDev[k]
  dfStockRisk1$ACStDev[i] <- ACStDev[k]
  dfStockRisk1$CCSkewness[i] <- CCSkewness[k]
  dfStockRisk1$DCSkewness[i] <- DCSkewness[k]
  dfStockRisk1$ACSkewness[i] <- ACSkewness[k]
  dfStockRisk1$CCKurtosis[i] <- CCKurtosis[k]
  dfStockRisk1$DCKurtosis[i] <- DCKurtosis[k]
  dfStockRisk1$ACKurtosis[i] <- ACKurtosis[k]
# Estimates of dollar change << OLD LIKELY DELETE >>
  dfStockRisk1$ACCCStDev[i] <- CCStDev[k]*dfStockRisk1$Price[i]
  dfStockRisk1$ACDCStDev[i] <- DCStDev[k]*dfStockRisk1$Price[i]
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
  if(!is.na(dfStockRisk1$ACCCStDev[i])){
    k = k + 1
    X[k] <- dfStockRisk1$Date[i]
    Y1[k] <- dfStockRisk1$ACCCStDev[i]
    Y2[k] <- dfStockRisk1$ACDCStDev[i]
    Y3[k] <- dfStockRisk1$ACStDev[i]
    Y4[k] <- dfStockRisk1$ACStDev[i] - dfStockRisk1$ACCCStDev[i]
    Y5[k] <- dfStockRisk1$ACStDev[i] - dfStockRisk1$ACDCStDev[i]
    X2[k] <- dfStockRisk1$Price[i]
    Y6[k] <- dfStockRisk1$CCMean[i]
    Y7[k] <- dfStockRisk1$DCMean[i]
    Y8[k] <- dfStockRisk1$ACMean[i]/dfStockRisk1$Price[i] # Normalized
    Y9[k] <- dfStockRisk1$CCStDev[i]
    Y10[k] <- dfStockRisk1$DCStDev[i]
    Y11[k] <- dfStockRisk1$ACStDev[i]/dfStockRisk1$Price[i] # Normalized
    Y12[k] <- dfStockRisk1$CCSkewness[i]
    Y13[k] <- dfStockRisk1$DCSkewness[i]
    Y14[k] <- dfStockRisk1$ACSkewness[i]
    Y15[k] <- dfStockRisk1$CCKurtosis[i]
    Y16[k] <- dfStockRisk1$DCKurtosis[i]
    Y17[k] <- dfStockRisk1$ACKurtosis[i]
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
plot(X, X2, type = "p", main = mTitle1,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
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
plot(X, Y6, type = "p", main = mTitle1,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y7, type = "p", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
lines(X, Y8, type = "p", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 3, cex = 0.5)
legend("bottomleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
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
plot(X, Y9, type = "b", main = mTitle1,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y10, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
lines(X, Y11, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
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
plot(X, Y12, type = "b", main = mTitle1,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y13, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
lines(X, Y14, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 3, cex = 0.5)
legend("bottomleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
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
plot(X, Y15, type = "b", main = mTitle1,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y16, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
lines(X, Y17, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
