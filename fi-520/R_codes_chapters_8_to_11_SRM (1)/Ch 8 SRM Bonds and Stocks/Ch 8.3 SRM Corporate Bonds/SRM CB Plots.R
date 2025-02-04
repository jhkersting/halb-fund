# SRM UST Plots.R
#
# Level Duration - Horizon
#
x <- Maturity
xTitle = "Maturity"
y <- EffDuration
y2 <- AIMDLevel
y3 <- BCMDLevel
y4 <- SCMDLevel
yTitle = "Level Duration (At Horizon)"
legtxt = c("Effective", "All-In Level","Base Curve Level", "Spread Curve Level")
lTitle = "Variable"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, y2, y3, y4, na.rm=TRUE)
MinValueY = min(y, y2, y3, y4, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 3, 
  cex = 0.5)
lines(x, y4, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 4, 
  cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(2,2,2,2), 
  col = c("black","black","black","black"), pch = c(1,2,3,4), bty = "n", 
  title = lTitle)
#
# Slope Duration - Horizon
#
x <- Maturity
xTitle = "Maturity"
y <- AIMDSlope
y2 <- BCMDSlope
y3 <- SCMDSlope
yTitle = "Slope Duration (At Horizon)"
legtxt = c("All-In Slope","Base Curve Slope", "Spread Curve Slope")
lTitle = "Variable"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, y2, y3, na.rm=TRUE)
MinValueY = min(y, y2, y3, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 3, 
  cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#
# Curvature1 Duration - Horizon
#
x <- Maturity
xTitle = "Maturity"
y <- AIMDCurvature1
y2 <- BCMDCurvature1
y3 <- SCMDCurvature1
yTitle = "Curvature1 Duration (At Horizon)"
legtxt = c("All-In Curvature1","Base Curve Curvature1", "Spread Curve Curvature1")
lTitle = "Variable"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, y2, y3, na.rm=TRUE)
MinValueY = min(y, y2, y3, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 3, 
  cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#
# Level Convexity - Horizon
#
x <- Maturity
xTitle = "Maturity"
y <- EffConvexity
y2 <- AICYLevel
y3 <- BCCYLevel
y4 <- SCCYLevel
yTitle = "Level Convexity (At Horizon)"
legtxt = c("Effective", "All-In Level","Base Curve Level", "Spread Curve Level")
lTitle = "Variable"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, y2, y3, y4, na.rm=TRUE)
MinValueY = min(y, y2, y3, y4, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 3, 
  cex = 0.5)
lines(x, y4, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 4, 
  cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(2,2,2,2), 
  col = c("black","black","black","black"), pch = c(1,2,3,4), bty = "n", 
  title = lTitle)
#
# Slope Convexity - Horizon
#
x <- Maturity
xTitle = "Maturity"
y <- AICYSlope
y2 <- BCCYSlope
y3 <- SCCYSlope
yTitle = "Slope Convexity (At Horizon)"
legtxt = c("All-In Slope","Base Curve Slope", "Spread Curve Slope")
lTitle = "Variable"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, y2, y3, na.rm=TRUE)
MinValueY = min(y, y2, y3, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 3, 
  cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#
# Curvature1 Convexity - Horizon
#
x <- Maturity
xTitle = "Maturity"
y <- AICYCurvature1
y2 <- BCCYCurvature1
y3 <- SCCYCurvature1
yTitle = "Curvature1 Convexity (At Horizon)"
legtxt = c("All-In Curvature1","Base Curve Curvature1", "Spread Curve Curvature1")
lTitle = "Variable"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, y2, y3, na.rm=TRUE)
MinValueY = min(y, y2, y3, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 3, 
  cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#
# Cross Convexity - Horizon - Level and Slope
#
x <- Maturity
xTitle = "Maturity"
y <- AICCLevelSlope
y2 <- BCCCLevelSlope
y3 <- SCCCLevelSlope
yTitle = "Cross Convexity (At Horizon)"
legtxt = c("All-In Level/Slope","Base Curve Level/Slope", "Spread Curve Level/Slope")
lTitle = "Variable"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, y2, y3, na.rm=TRUE)
MinValueY = min(y, y2, y3, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 3, 
  cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#
# Cross Convexity - Horizon - Level and Curvature1
#
x <- Maturity
xTitle = "Maturity"
y <- AICCLevelCurvature1
y2 <- BCCCLevelCurvature1
y3 <- SCCCLevelCurvature1
yTitle = "Cross Convexity (At Horizon)"
legtxt = c("All-In Level/Curvature1","Base Curve Level/Curvature1", "Spread Curve Level/Curvature1")
lTitle = "Variable"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, y2, y3, na.rm=TRUE)
MinValueY = min(y, y2, y3, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 3, 
  cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#
# Cross Convexity - Horizon - Slope and Curvature1
#
x <- Maturity
xTitle = "Maturity"
y <- AICCSlopeCurvature1
y2 <- BCCCSlopeCurvature1
y3 <- SCCCSlopeCurvature1
yTitle = "Cross Convexity (At Horizon)"
legtxt = c("All-In Slope/Curvature1","Base Curve Slope/Curvature1", "Spread Curve Slope/Curvature1")
lTitle = "Variable"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, y2, y3, na.rm=TRUE)
MinValueY = min(y, y2, y3, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
lines(x, y3, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 3, 
  cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#
# HPR - Horizon
#
x <- Maturity
xTitle = "Maturity"
y <- HPRH
y2 <- HPRH2
yTitle = "Horizon Holding Period Return"
legtxt = c("All-In","Two Curves")
lTitle = "Variable"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, y2, na.rm=TRUE)
MinValueY = min(y, y2, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1), lty = c(0,0), 
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)

