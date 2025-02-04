# Arb Plots.R
#
# Arb1 Plot
#
if(ArbPlot == 1){
  if(CAAFixedBounds){
    MaxValueY = CAAUpperBound
    MinValueY = CAALowerBound
  } else {
    MaxValueY = max(y1, na.rm = TRUE)
    MinValueY = min(y1, na.rm = TRUE)
  }
  IncrementY = (as.numeric(MaxValueY) - as.numeric(MinValueY))/12.0
  ylim1 = c(1:2)
  ylim1[1] = MinValueY
  ylim1[2] = MaxValueY
  TickMarksY = c(seq(from=MinValueY,to=MaxValueY,by=IncrementY))
  lblY = paste0(format(round(TickMarksY,3), trim = TRUE, digits = 2, 
    justify = c("right"), width = 0, big.mark = ","))
  plot(x, y1, axes = FALSE,
    type = "p", main = mTitle, xlab = xTitle, ylab = yTitle, col = "black", 
    xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.75)
  box() # create a wrap around the points plotted
  axis(labels=NA,side=1,tck=-0.015,at=TickMarksX) 
  axis(lwd=0,side=1,line=-0.5,at=TickMarksX, label=lblX) 
  axis(labels=NA,side=2,tck=-0.015, at=TickMarksY)
  axis(lwd=0,line=-0.5,side=2,las=1,at=TickMarksY, label = lblY) 
  legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
    col = c("black", "black"), pch = c(1, 2), bty = "n", title = lTitle)
}
if(ArbPlot == 2){
  if(CAAFixedBounds){
    MaxValueY = CAAUpperBound
    MinValueY = CAALowerBound
  } else {
    MaxValueY = max(y1, y2, na.rm = TRUE)
    MinValueY = min(y1, y2, na.rm = TRUE)
  }
  IncrementY = (as.numeric(MaxValueY) - as.numeric(MinValueY))/12.0
  ylim1 = c(1:2)
  ylim1[1] = MinValueY
  ylim1[2] = MaxValueY
  TickMarksY = c(seq(from=MinValueY,to=MaxValueY,by=IncrementY))
  lblY = paste0(format(round(TickMarksY,3), trim = TRUE, digits = 2, 
    justify = c("right"), width = 0, big.mark = ","))
  plot(x, y1, axes = FALSE,
    type = "p", main = mTitle, xlab = xTitle, ylab = yTitle, col = "black", 
    xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
  lines(x, y2,  
    type = "p", col = "black", xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
  box() # create a wrap around the points plotted
  axis(labels=NA,side=1,tck=-0.015,at=TickMarksX) 
  axis(lwd=0,side=1,line=-0.5,at=TickMarksX, label=lblX) 
  axis(labels=NA,side=2,tck=-0.015, at=TickMarksY)
  axis(lwd=0,line=-0.5,side=2,las=1,at=TickMarksY, label = lblY) 
  legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
    col = c("black", "black"), pch = c(1, 2), bty = "n", title = lTitle)
}
