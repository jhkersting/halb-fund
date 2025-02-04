# SRM Two Underlying Instruments Test.R
# Illustrate risk statistics for two instruments
# rmarkdown::render("SRM Two Underlying Instruments Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
#  openxlsx - manipulate spreadsheet files
Packages <- c("date", "zoo", "openxlsx", "stats", "moments") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#
# Import large data files so we trim with start and end date
#
# StartDate = 20150101
# EndDate = 20200630
StartDate = 20150101
EndDate = 20221216
RollingWindow = 130 # 1/2 year (260 trading days per year)
# Instrument #1
FileName <- "WEN.PRN"
mTitle1 <- paste0("Wendy's")
source('Instrument 1.R')
dfResults1 = dfStockRisk1
# Instrument #2 -- can reference different files, but same format
FileName <- "MCD.PRN"
mTitle2 <- paste0("McDonalds")
source('Instrument 2.R') # Use with same File, but different contracts
dfResults2 = dfStockRisk2
mTitle = paste0(mTitle1, " and ", mTitle2)
#
# Price
#
yTitle = "Price (Normalized to $1 Initially)"
X = as.date(dfResults1$Date)
P1 = as.double(dfResults1$Price[1])
P2 = as.double(dfResults2$Price[1])
Y1 = dfResults1$Price/P1
Y2 = dfResults2$Price/P2
MaxValue = max(X); MinValue = min(X)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
MaxValue = max(Y1, Y2); MinValue = min(Y1, Y2)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
xTitle = "Date"
legtxt = c(mTitle1, mTitle2)
lTitle = "Parameter"
plot(X, Y1, type = "b", main = mTitle,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y2, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1), 
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
#
# Mean (CC)
#
yTitle = "Mean (CC)"
X = as.date(dfResults1$Date)
XY = as.data.frame(X)
XY$Y1 = dfResults1$CCMean
XY$Y2 = dfResults2$CCMean
XY = na.omit(XY)
X <- XY$X
Y1 <- XY$Y1
Y2 <- XY$Y2
Y3 = Y1 - Y2
MaxValue = max(X); MinValue = min(X)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
MaxValue = max(Y1, Y2, na.rm = TRUE); MinValue = min(Y1, Y2, na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
xTitle = "Date"
legtxt = c(mTitle1, mTitle2)
lTitle = "Parameter"
plot(X, Y1, type = "b", main = mTitle,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y2, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
legend("bottomleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1), 
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
yTitle = 'Difference in Mean'
MaxValue = max(Y3, na.rm = TRUE); MinValue = min(Y3, na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
plot(X, Y3, type = "b", main = mTitle,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
#
# Standard Deviation (CC)
#
yTitle = "Standard Deviation (CC)"
X = as.date(dfResults1$Date)
XY = as.data.frame(X)
XY$Y1 = dfResults1$CCStDev
XY$Y2 = dfResults2$CCStDev
XY = na.omit(XY)
X <- XY$X
Y1 <- XY$Y1
Y2 <- XY$Y2
Y3 = Y1 - Y2
MaxValue = max(X); MinValue = min(X)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
MaxValue = max(Y1, Y2, na.rm = TRUE); MinValue = min(Y1, Y2, na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
xTitle = "Date"
legtxt = c(mTitle1, mTitle2)
lTitle = "Parameter"
plot(X, Y1, type = "b", main = mTitle,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y2, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1), 
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
yTitle = 'Difference in Standard Deviation'
MaxValue = max(Y3, na.rm = TRUE); MinValue = min(Y3, na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
plot(X, Y3, type = "b", main = mTitle,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
#
# Skewness (CC)
#
yTitle = "Skewness (CC)"
X = as.date(dfResults1$Date)
XY = as.data.frame(X)
XY$Y1 = dfResults1$CCSkewness
XY$Y2 = dfResults2$CCSkewness
XY = na.omit(XY)
X <- XY$X
Y1 <- XY$Y1
Y2 <- XY$Y2
Y3 = Y1 - Y2
MaxValue = max(X); MinValue = min(X)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
MaxValue = max(Y1, Y2, na.rm = TRUE); MinValue = min(Y1, Y2, na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
xTitle = "Date"
legtxt = c(mTitle1, mTitle2)
lTitle = "Parameter"
plot(X, Y1, type = "b", main = mTitle,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y2, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1), 
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
yTitle = 'Difference in Skewness'
MaxValue = max(Y3, na.rm = TRUE); MinValue = min(Y3, na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
plot(X, Y3, type = "b", main = mTitle,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
#
# Kurtosis (CC)
#
yTitle = "Kurtosis (CC)"
X = as.date(dfResults1$Date)
XY = as.data.frame(X)
XY$Y1 = dfResults1$CCKurtosis
XY$Y2 = dfResults2$CCKurtosis
XY = na.omit(XY)
X <- XY$X
Y1 <- XY$Y1
Y2 <- XY$Y2
Y3 = Y1 - Y2
MaxValue = max(X); MinValue = min(X)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
MaxValue = max(Y1, Y2, na.rm = TRUE); MinValue = min(Y1, Y2, na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
xTitle = "Date"
legtxt = c(mTitle1, mTitle2)
lTitle = "Parameter"
plot(X, Y1, type = "b", main = mTitle,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(X, Y2, type = "b", col ="black", xlim = xlim1, ylim = ylim1,
  pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1), 
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
yTitle = 'Difference in Kurtosis'
MaxValue = max(Y3, na.rm = TRUE); MinValue = min(Y3, na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
plot(X, Y3, type = "b", main = mTitle,
  xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)

