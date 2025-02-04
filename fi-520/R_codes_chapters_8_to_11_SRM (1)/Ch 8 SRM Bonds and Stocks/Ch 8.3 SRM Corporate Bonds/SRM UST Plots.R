# SRM UST Plots.R
# 
# Analysis of UST bonds
#
# Asked yield and maturity
#
x <- Maturity
xTitle = "Maturity"
y <- UST$ASKED.YIELD
yTitle = "Asked Yield to Maturity"
MaxXValue = max(x, na.rm=TRUE)
MinXValue = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MaxYValue = max(y, na.rm=TRUE)
MinYValue = min(y, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
plot(x, y, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# #
# # Effective Duration
# #
# x <- Maturity
# xTitle = "Maturity"
# y <- EffDuration 
# yTitle = "Effective Duration"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# #
# # Effective Convexity
# #
# x <- Maturity
# xTitle = "Maturity"
# y <- EffConvexity
# yTitle = "Effective Convexity"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# #
# # Relative error plot - LSC DF fit compared with invoice amount
# #
# x <- Maturity
# xTitle = "Maturity"
# y <- RelativeBVError 
# yTitle = "Relative Bond Value Error"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# #
# # Relative Bond Value Error
# #
# x <- MacDuration
# xTitle = "Macaulay Duration"
# y <- RelativeBVError 
# yTitle = "Relative Bond Value Error"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# #
# # Absolute error plot
# #
# x <- Maturity
# xTitle = "Maturity"
# y <- AbsoluteBVError 
# yTitle = "Absolute Bond Value Error"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# #
# # Absolute Bond Value Error and Duration
# #
# x <- MacDuration
# xTitle = "Macaulay Duration"
# y <- AbsoluteBVError 
# yTitle = "Absolute Bond Value Error"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# #
# # Yield differential plot
# #
# x <- Maturity
# xTitle = "Maturity"
# y <- YieldDiffBPs 
# yTitle = "Yield Differential in Basis Points"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(Maturity, YieldDiffBPs, type = "p", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# #
# # Yield Differential and Duration
# #
# x <- MacDuration
# xTitle = "Macaulay Duration"
# y <- YieldDiffBPs 
# yTitle = "Yield Differential in Basis Points"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# #
# # Asked Yield and Maturity
# #
# x <- Maturity
# xTitle = "Maturity"
# y <- UST$ASKED.YIELD
# yTitle = "Asked Yield to Maturity"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# #
# # Asked Yield and Macaulay Duration
# #
# x <- MacDuration
# xTitle = "Macaulay Duration"
# y <- UST$ASKED.YIELD
# yTitle = "Asked Yield to Maturity"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# #
# # Duration and convexity
# #
# x <- MacDuration
# xTitle = "Macaulay Duration"
# y <- StdConvexity
# yTitle = "Standard Convexity"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# #
# # Duration error
# #
# x <- Maturity
# xTitle = "Maturity"
# y <- EffDuration - MacDuration
# yTitle = "Effective Less Macaulay Duration"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# #
# # Convexity error
# #
# x <- Maturity
# xTitle = "Maturity"
# y <- EffConvexity - StdConvexity
# yTitle = "Effective Less Standard Convexity"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
#
# LSC - Modified Duration - Horizon
#
x <- Maturity
xTitle = "Maturity"
y <- LSCMDLevel
y2 <- EffDuration
yTitle = "Level Duration and Mod. Duration"
legtxt = c("LSC Mod. Duration (Level)","Mod. Duration")
lTitle = "Variable"
MaxXValue = max(x, na.rm=TRUE)
MinXValue = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MaxYValue = max(y, y2, na.rm=TRUE)
MinYValue = min(y, y2, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1), lty = c(2,2), 
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
#
# LSC Analysis (SRM)
#
#
# LSC - Modified Duration - Horizon - Level, Slope, Curve1
#
x <- Maturity
xTitle = "Maturity"
y <- LSCMDLevel
y2 <- LSCMDSlope
y3 <- LSCMDCurve1
yTitle = "LSC Modified Duration"
legtxt = c("Level","Slope", "Curve1")
lTitle = "Variable"
MaxXValue = max(x, na.rm=TRUE)
MinXValue = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MaxYValue = max(y, y2, y3, na.rm=TRUE)
MinYValue = min(y, y2, y3, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(3,3,3), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
#
# LSC - Convexity - Horizon - Level, Slope, Curve1
#
x <- Maturity
xTitle = "Maturity"
y <- LSCCYLevel
y2 <- LSCCYSlope
y3 <- LSCCYCurve1
yTitle = "LSC Convexity"
legtxt = c("Level","Slope", "Curve1")
lTitle = "Variable"
MaxXValue = max(x, na.rm=TRUE)
MinXValue = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MaxYValue = max(y, y2, y3, na.rm=TRUE)
MinYValue = min(y, y2, y3, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(3,3,3), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
#
# LSC - Cross Convexity - Horizon - Level/Slope, Level/Curve1
#  Slope/Curve1
#
x <- Maturity
xTitle = "Maturity"
y <- LSCCCLevelSlope
y2 <- LSCCCLevelCurve1
y3 <- LSCCCSlopeCurve1
yTitle = "LSC Cross Convexity"
legtxt = c("Level/Slope","Level/Curve1", "Slope/Curve1")
lTitle = "Variable"
MaxXValue = max(x, na.rm=TRUE)
MinXValue = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MaxYValue = max(y, y2, y3, na.rm=TRUE)
MinYValue = min(y, y2, y3, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(3,3,3), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
#
# HPR - Horizon
#
x <- Maturity
xTitle = "Maturity"
y <- HPRH
yTitle = "Horizon Holding Period Return"
MaxXValue = max(x, na.rm=TRUE)
MinXValue = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MaxYValue = max(y, na.rm=TRUE)
MinYValue = min(y, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
