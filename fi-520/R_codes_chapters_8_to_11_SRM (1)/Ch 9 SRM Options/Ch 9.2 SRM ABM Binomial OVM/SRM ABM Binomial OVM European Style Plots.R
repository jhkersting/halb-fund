# SRM ABM Binomial OVM European Style Plots.R
# Plots: Plain Vanilla Options with respect to the Stock Price
MaxValue = max(TStockPrice); MinValue = min(TStockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
#  Call with boundaries
MaxValue = max(ESCallValue, ESCallLB, CallIV)
MinValue = min(ESCallValue, ESCallLB, CallIV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Call Value", "Call Lower Bound Value", "Call Intrinsic Value")
mTitle = "European-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Call Value"
lTitle = "Parameter"
plot(TStockPrice, ESCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESCallLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, CallIV, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
  col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
  title = lTitle)
# Put with boundaries
MaxValue = max(ESPutValue, ESPutLB, PutIV)
MinValue = min(ESPutValue, ESPutLB, PutIV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Put Value", "Put Lower Bound Value", "Put Intrinsic Value")
mTitle = "European-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Put Value"
lTitle = "Parameter"
plot(TStockPrice, ESPutValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESPutLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, PutIV, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
  col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
  title = lTitle)
# Plots: Plain Vanilla Options with respect to the Stock Price
MaxValue = max(TStockPrice); MinValue = min(TStockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
#  Call with boundaries
MaxValue = max(ESCallValue, ESCallLB, ESCallUB)
MinValue = min(ESCallValue, ESCallLB, ESCallUB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Call Value", "Call Lower Bound", "Call Upper Bound")
mTitle = "European-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Call Value"
lTitle = "Parameter"
plot(TStockPrice, ESCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESCallLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESCallUB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
  col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
  title = lTitle)
# Put with boundaries
MaxValue = max(ESPutValue, ESPutLB, ESPutUB)
MinValue = min(ESPutValue, ESPutLB, ESPutUB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Put Value", "Put Lower Bound", "Put Upper Bound")
mTitle = "European-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Put Value"
lTitle = "Parameter"
plot(TStockPrice, ESPutValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESPutLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutUB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("right", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black", "black", "black"), pch = c(NA,NA,NA), bty = "n",
  title = lTitle)
# Calls and puts
MaxValue = max(ESCallValue, ESPutValue)
MinValue = min(ESCallValue, ESPutValue)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(TStockPrice); MinValue = min(TStockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Call Value","Put Value")
mTitle = "European-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Option Value"
lTitle = "Parameter"
plot(TStockPrice, ESCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESPutValue, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(0, 0),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
#
#  Call Deltas
#
MaxValue = max(ESCallDeltaDirect, ESCallDeltaDirectEnh, ESCallDeltaNG)
MinValue = min(ESCallDeltaDirect, ESCallDeltaDirectEnh, ESCallDeltaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "European-Style Binomial Call Option Deltas (ABM)"
xTitle = "Stock Price"
yTitle = "Delta"
lTitle = "Parameter"
plot(TStockPrice, ESCallDeltaDirect, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESCallDeltaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESCallDeltaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
  col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
  title = lTitle)



#
#  Call Deltas
#
xlim1 = c(1:2); xlim1[1] = 95; xlim1[2] = 105
MaxValue = 0.65
MinValue = 0.45
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "European-Style Binomial Call Option Deltas (ABM)"
xTitle = "Stock Price"
yTitle = "Delta"
lTitle = "Parameter"
plot(TStockPrice, ESCallDeltaDirect, type = "b", main = mTitle,
     sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESCallDeltaDirectEnh, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESCallDeltaNG, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
       col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
       title = lTitle)

MaxValue = max(TStockPrice); MinValue = min(TStockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
#
#  Put Deltas
#
MaxValue = max(ESPutDeltaDirect, ESPutDeltaDirectEnh, ESPutDeltaNG)
MinValue = min(ESPutDeltaDirect, ESPutDeltaDirectEnh, ESPutDeltaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "European-Style Binomial Put Option Deltas (ABM)"
xTitle = "Stock Price"
yTitle = "Delta"
lTitle = "Parameter"
plot(TStockPrice, ESPutDeltaDirect, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESPutDeltaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutDeltaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
  col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
  title = lTitle)
#
#  Call Gammas
#
MaxValue = max(ESCallGammaDirect, ESCallGammaDirectEnh, ESCallGammaNG)
MinValue = min(ESCallGammaDirect, ESCallGammaDirectEnh, ESCallGammaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "European-Style Binomial Call Option Gammas (ABM)"
xTitle = "Stock Price"
yTitle = "Gamma"
lTitle = "Parameter"
plot(TStockPrice, ESCallGammaDirect, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESCallGammaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESCallGammaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
  col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
  title = lTitle)
#
#  Put Gammas
#
MaxValue = max(ESPutGammaDirect, ESPutGammaDirectEnh, ESPutGammaNG)
MinValue = min(ESPutGammaDirect, ESPutGammaDirectEnh, ESPutGammaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "European-Style Binomial Put Option Gammas (ABM)"
xTitle = "Stock Price"
yTitle = "Gamma"
lTitle = "Parameter"
plot(TStockPrice, ESPutGammaDirect, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESPutGammaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutGammaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
  col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
  title = lTitle)
#
#  Call Thetas
#
MaxValue = max(ESCallThetaDirect, ESCallThetaDirectEnh, ESCallThetaNG)
MinValue = min(ESCallThetaDirect, ESCallThetaDirectEnh, ESCallThetaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "European-Style Binomial Call Option Thetas (ABM)"
xTitle = "Stock Price"
yTitle = "Theta"
lTitle = "Parameter"
plot(TStockPrice, ESCallThetaDirect, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESCallThetaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESCallThetaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
  col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
  title = lTitle)
#
#  Put Thetas
#
MaxValue = max(ESPutThetaDirect, ESPutThetaDirectEnh, ESPutThetaNG)
MinValue = min(ESPutThetaDirect, ESPutThetaDirectEnh, ESPutThetaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "European-Style Binomial Put Option Thetas (ABM)"
xTitle = "Stock Price"
yTitle = "Theta"
lTitle = "Parameter"
plot(TStockPrice, ESPutThetaDirect, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESPutThetaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutThetaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
  col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
  title = lTitle)
#
#  Vegas
#
MaxValue = max(ESCallVegaNG, ESPutVegaNG)
MinValue = min(ESCallVegaNG, ESPutVegaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Call", "Put")
mTitle = "European-Style Binomial Option Vegas (ABM)"
xTitle = "Stock Price"
yTitle = "Vega"
lTitle = "Parameter"
plot(TStockPrice, ESCallVegaNG, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESPutVegaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(0, 0),
  col = c("black", "black"), pch = c(1, 2), bty = "n", title = lTitle)
#
#  Rho
#
MaxValue = max(ESCallRhoNG, ESPutRhoNG)
MinValue = min(ESCallRhoNG, ESPutRhoNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Call", "Put")
mTitle = "European-Style Binomial Option Rhos (ABM)"
xTitle = "Stock Price"
yTitle = "Rho"
lTitle = "Parameter"
plot(TStockPrice, ESCallRhoNG, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESPutRhoNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(0, 0),
  col = c("black", "black"), pch = c(1, 2), bty = "n", title = lTitle)



# # Plain vanilla option values with respect to the number of steps
# xlim1 = c(1:2); xlim1[1] = MinStep; xlim1[2] = MaxStep
# MaxOptionValue = max(ESCallValue,ESPutValue)
# MinOptionValue = min(ESCallValue,ESPutValue)
# ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
# legtxt = c("Call Value","Put Value")
# mTitle = "European-Style Binomial Plain Vanilla Option Value (ABM)"
# xTitle = "Number of Steps"
# yTitle = "Plain Vanilla Option Value"
# lTitle = "Parameter"
# plot(NumberOfSteps, ESPutValue, type="b",
#   main=mTitle, sub=sTitleBIN,
#   xlab="Number Of Steps", ylab="Option Values", col="black",
#   xlim = xlim1, ylim = ylim1)
# lines(NumberOfSteps, ESCallValue, col="black", xlim = xlim1, ylim = ylim1)
# legend("right", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("black","black"), pch = c(NA,NA), bty = "n", title = lTitle)
# # Digital option values with respect to the number of steps
# MaxOptionValue = max(DigitalCallValue, DigitalPutValue)
# MinOptionValue = min(DigitalCallValue, DigitalPutValue)
# ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
# legtxt = c("Call Value","Put Value")
# mTitle = "European-Style Binomial Digital Option Value (ABM)"
# xTitle = "Stock Price"
# yTitle = "Digital Option Value"
# lTitle = "Parameter"
# plot(NumberOfSteps, DigitalCallValue, type="b",
#   main=mTitle, sub=sTitleBINDP,
#   xlab="Number Of Steps", ylab="Digital Option Values",
#   col="black", xlim = xlim1, ylim = ylim1)
# lines(NumberOfSteps, DigitalPutValue, col="black", xlim = xlim1, ylim = ylim1)
# legend("topright", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("black","black"), pch = c(NA,NA), bty = "n", title = lTitle)
# 
# 
# # Digital Options with respect to the Stock Price
# MaxValue = max(DigitalCallValue, DigitalPutValue)
# MinValue = min(DigitalCallValue, DigitalPutValue)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# MaxValue = max(TStockPrice); MinValue = min(TStockPrice)
# xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
# legtxt = c("Call Value","Put Value")
# mTitle = "European-Style Binomial Digital Option Value (ABM)"
# xTitle = "Stock Price"
# yTitle = "Digital Option Value"
# lTitle = "Parameter"
# plot(TStockPrice, DigitalCallValue, type = "b", main = mTitle,
#      sub = sTitleBINDP, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#      ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, DigitalPutValue, type = "b", col ="black", xlim = xlim1,
#       ylim = ylim1, pch = 2, cex = 0.5)
# legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#        col = c("black","black"), pch = c(NA,NA), bty = "n", title = lTitle)

