# SRM Basic Plots.R
#
# Asked yield to maturity and maturity
#
x <- Maturity
xTitle = "Maturity"
y <- YieldToMaturity
yTitle = "Asked Yield to Maturity (%)"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, na.rm=TRUE)
MinValueY = min(y, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
#
# Asked yield to maturity and effective duration
#
x <- EffDuration
xTitle = "Effective Duration"
y <- YieldToMaturity
yTitle = "Asked Yield to Maturity (%)"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, na.rm=TRUE)
MinValueY = min(y, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
#
# Relative bond value error and effective duration
#
x <- EffDuration
xTitle = "Effective Duration"
y <- RelativeBVError
y2 <- RelativeBVError2
yTitle = "Relative Bond Value Error (%)"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, y2, na.rm=TRUE)
MinValueY = min(y, y2, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
legtxt = c("All-In LSC Model","Two LSC Models")
lTitle = "Variable"
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
lines(x, y2, type = "p", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(2,2,2), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#
# Effective duration and maturity
#
x <- Maturity
xTitle = "Maturity"
y <- EffDuration
yTitle = "Effective Duration"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, na.rm=TRUE)
MinValueY = min(y, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
#
# Effective convexity and effective duration
#
x <- EffDuration
xTitle = "Effective Duration"
y <- EffConvexity
yTitle = "Effective Convexity"
MaxValueX = max(x, na.rm=TRUE)
MinValueX = min(0.0, x, na.rm=TRUE)
xlim1 = c(1:2); xlim1[1] = MinValueX; xlim1[2] = MaxValueX
MaxValueY = max(y, na.rm=TRUE)
MinValueY = min(y, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle, 
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
