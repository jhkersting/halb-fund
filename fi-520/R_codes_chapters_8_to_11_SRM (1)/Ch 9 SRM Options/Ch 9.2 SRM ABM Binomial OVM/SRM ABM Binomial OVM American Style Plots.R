# SRM ABM Binomial OVM American Style Plots.R
# # Plots: American-Style Options with respect to the Stock Price
MaxValue = max(TStockPrice); MinValue = min(TStockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
#  Call with boundaries
MaxValue = max(ASCallValue, ESCallLB, ASCallLB)
MinValue = min(ASCallValue, ESCallLB, ASCallLB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Call Value", "Call Lower Bound Value", "Call Intrinsic Value")
mTitle = "American-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Call Value"
lTitle = "Parameter"
plot(TStockPrice, ASCallValue, type = "b", main = mTitle,
     sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESCallLB, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ASCallLB, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
       col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
       title = lTitle)
# Put with boundaries
MaxValue = max(ASPutValue, ESPutLB, ASPutLB)
MinValue = min(ASPutValue, ESPutLB, ASPutLB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Put Value", "Put Lower Bound Value", "Put Intrinsic Value")
mTitle = "American-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Put Value"
lTitle = "Parameter"
plot(TStockPrice, ASPutValue, type = "b", main = mTitle,
     sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ESPutLB, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ASPutLB, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 3, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
       col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
       title = lTitle)
# Plots: Plain Vanilla Options with respect to the Stock Price
MaxValue = max(TStockPrice); MinValue = min(TStockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
#  Call with boundaries
MaxValue = max(ASCallValue, ASCallLB, ASCallUB)
MinValue = min(ASCallValue, ASCallLB, ASCallUB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Call Value", "Call Lower Bound", "Call Upper Bound")
mTitle = "American-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Call Value"
lTitle = "Parameter"
plot(TStockPrice, ASCallValue, type = "b", main = mTitle,
     sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallLB, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ASCallUB, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
       col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
       title = lTitle)
# Put with boundaries
MaxValue = max(ASPutValue, ASPutLB, ASPutUB)
MinValue = min(ASPutValue, ASPutLB, ASPutUB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Put Value", "Put Lower Bound", "Put Upper Bound")
mTitle = "American-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Put Value"
lTitle = "Parameter"
plot(TStockPrice, ASPutValue, type = "b", main = mTitle,
     sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASPutLB, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ASPutUB, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 3, cex = 0.5)
legend("right", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
       col = c("black", "black", "black"), pch = c(NA,NA,NA), bty = "n",
       title = lTitle)
# Calls and puts
MaxValue = max(ASCallValue, ASPutValue)
MinValue = min(ASCallValue, ASPutValue)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(TStockPrice); MinValue = min(TStockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Call Value","Put Value")
mTitle = "American-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Option Value"
lTitle = "Parameter"
plot(TStockPrice, ASCallValue, type = "b", main = mTitle,
     sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASPutValue, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(0, 0),
       col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
#
#  Call Deltas
#
MaxValue = max(ASCallDeltaDirect, ASCallDeltaDirectEnh, ASCallDeltaNG)
MinValue = min(ASCallDeltaDirect, ASCallDeltaDirectEnh, ASCallDeltaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "American-Style Binomial Call Option Deltas (ABM)"
xTitle = "Stock Price"
yTitle = "Delta"
lTitle = "Parameter"
plot(TStockPrice, ASCallDeltaDirect, type = "b", main = mTitle,
     sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallDeltaDirectEnh, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ASCallDeltaNG, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
       col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
       title = lTitle)
#
#  Put Deltas
#
MaxValue = max(ASPutDeltaDirect, ASPutDeltaDirectEnh, ASPutDeltaNG)
MinValue = min(ASPutDeltaDirect, ASPutDeltaDirectEnh, ASPutDeltaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "American-Style Binomial Put Option Deltas (ABM)"
xTitle = "Stock Price"
yTitle = "Delta"
lTitle = "Parameter"
plot(TStockPrice, ASPutDeltaDirect, type = "b", main = mTitle,
     sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASPutDeltaDirectEnh, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ASPutDeltaNG, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
       col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
       title = lTitle)
#
#  Call Gammas
#
MaxValue = max(ASCallGammaDirect, ASCallGammaDirectEnh, ASCallGammaNG)
MinValue = min(ASCallGammaDirect, ASCallGammaDirectEnh, ASCallGammaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "American-Style Binomial Call Option Gammas (ABM)"
xTitle = "Stock Price"
yTitle = "Gamma"
lTitle = "Parameter"
plot(TStockPrice, ASCallGammaDirect, type = "b", main = mTitle,
     sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallGammaDirectEnh, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ASCallGammaNG, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
       col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
       title = lTitle)
#
#  Put Gammas
#
MaxValue = max(ASPutGammaDirect, ASPutGammaDirectEnh, ASPutGammaNG)
MinValue = min(ASPutGammaDirect, ASPutGammaDirectEnh, ASPutGammaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "American-Style Binomial Put Option Gammas (ABM)"
xTitle = "Stock Price"
yTitle = "Gamma"
lTitle = "Parameter"
plot(TStockPrice, ASPutGammaDirect, type = "b", main = mTitle,
     sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASPutGammaDirectEnh, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ASPutGammaNG, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
       col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
       title = lTitle)
#
#  Call Thetas
#
MaxValue = max(ASCallThetaDirect, ASCallThetaDirectEnh, ASCallThetaNG)
MinValue = min(ASCallThetaDirect, ASCallThetaDirectEnh, ASCallThetaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "American-Style Binomial Call Option Thetas (ABM)"
xTitle = "Stock Price"
yTitle = "Theta"
lTitle = "Parameter"
plot(TStockPrice, ASCallThetaDirect, type = "b", main = mTitle,
     sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallThetaDirectEnh, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ASCallThetaNG, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 3, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
       col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
       title = lTitle)
#
#  Put Thetas
#
MaxValue = max(ASPutThetaDirect, ASPutThetaDirectEnh, ASPutThetaNG)
MinValue = min(ASPutThetaDirect, ASPutThetaDirectEnh, ASPutThetaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Direct", "Direct Enhanced", "Numerical")
mTitle = "American-Style Binomial Put Option Thetas (ABM)"
xTitle = "Stock Price"
yTitle = "Theta"
lTitle = "Parameter"
plot(TStockPrice, ASPutThetaDirect, type = "b", main = mTitle,
     sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASPutThetaDirectEnh, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ASPutThetaNG, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 3, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(0, 0, 0),
       col = c("black", "black", "black"), pch = c(1, 2, 3), bty = "n",
       title = lTitle)
#
#  Vegas
#
MaxValue = max(ASCallVegaNG, ASPutVegaNG)
MinValue = min(ASCallVegaNG, ASPutVegaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Call", "Put")
mTitle = "American-Style Binomial Option Vegas (ABM)"
xTitle = "Stock Price"
yTitle = "Vega"
lTitle = "Parameter"
plot(TStockPrice, ASCallVegaNG, type = "b", main = mTitle,
     sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASPutVegaNG, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(0, 0),
       col = c("black", "black"), pch = c(1, 2), bty = "n", title = lTitle)
#
#  Rho
#
MaxValue = max(ASCallRhoNG, ASPutRhoNG)
MinValue = min(ASCallRhoNG, ASPutRhoNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Call", "Put")
mTitle = "American-Style Binomial Option Rhos (ABM)"
xTitle = "Stock Price"
yTitle = "Rho"
lTitle = "Parameter"
plot(TStockPrice, ASCallRhoNG, type = "b", main = mTitle,
     sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
     ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASPutRhoNG, type = "b", col ="black", xlim = xlim1,
      ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(0, 0),
       col = c("black", "black"), pch = c(1, 2), bty = "n", title = lTitle)






# MaxValue = max(TStockPrice); MinValue = min(TStockPrice)
# xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
# #  AS Call with boundaries
# MaxValue = max(ASCallValue, ASCallLB)
# MinValue = min(ASCallValue, ASCallLB)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Call Value", "Call Lower Bound Value")
# mTitle = "American-Style Binomial Option Value (ABM)"
# xTitle = "Stock Price"
# yTitle = "Call Value"
# lTitle = "Parameter"
# plot(TStockPrice, ASCallValue, type = "l", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASCallLB, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #  Calls (ES v. AS)
# MaxValue = max(ESCallValue, ASCallValue)
# MinValue = min(ESCallValue, ASCallValue)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("European-Style Call Value", "American-Style Call Value")
# mTitle = "Binomial Call Option Value (ABM)"
# xTitle = "Stock Price"
# yTitle = "Call Value"
# lTitle = "Parameter"
# plot(TStockPrice, ESCallValue, type = "l", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASCallValue, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #  Puts (ES v. AS)
# MaxValue = max(ESPutValue, ASPutValue)
# MinValue = min(ESPutValue, ASPutValue)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("European-Style Put Value", "American-Style Put Value")
# mTitle = "Binomial Put Option Value (ABM)"
# xTitle = "Stock Price"
# yTitle = "Put Value"
# lTitle = "Parameter"
# plot(TStockPrice, ESPutValue, type = "l", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASPutValue, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# # AS puts with boundaries
# MaxValue = max(ASPutValue, ASPutLB)
# MinValue = min(ASPutValue, ASPutLB)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Put Value", "Put Lower Bound Value")
# mTitle = "American-Style Binomial Option Value (ABM)"
# xTitle = "Stock Price"
# yTitle = "Put Value"
# lTitle = "Parameter"
# plot(TStockPrice, ASPutValue, type = "l", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASPutLB, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #  AS and ES call boundaries
# MaxValue = max(ESCallLB, ASCallLB)
# MinValue = min(ESCallLB, ASCallLB)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("European-Style", "American-Style")
# mTitle = "Call Option Lower Bounds"
# xTitle = "Stock Price"
# yTitle = "Boundary Value"
# lTitle = "Parameter"
# plot(TStockPrice, ESCallLB, type = "l", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASCallLB, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #  AS and ES put boundaries
# MaxValue = max(ESPutLB, ASPutLB)
# MinValue = min(ESPutLB, ASPutLB)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("European-Style", "American-Style")
# mTitle = "Put Option Lower Bounds"
# xTitle = "Stock Price"
# yTitle = "Boundary Value"
# lTitle = "Parameter"
# plot(TStockPrice, ESPutLB, type = "l", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASPutLB, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #  AS and ES call option values and boundaries
# MaxValue = max(ESCallValue, ESCallLB, ASCallValue, ASCallLB)
# MinValue = min(ESCallValue, ESCallLB, ASCallValue, ASCallLB)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("ES Call", "ES LB Call", "AS Call", "AS LB Call")
# mTitle = "Call Option Values and Lower Bounds"
# xTitle = "Stock Price"
# yTitle = "Values"
# lTitle = "Parameter"
# plot(TStockPrice, ESCallValue, type = "l", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ESCallLB, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TStockPrice, ASCallValue, type = "l", col ="green", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# lines(TStockPrice, ASCallLB, type = "l", col ="purple", xlim = xlim1,
#   ylim = ylim1, pch = 4, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1, 1),
#   lty = c(1, 1, 1, 1), col = c("blue", "red", "green", "purple"),
#   pch = c(NA,NA,NA), bty = "n", title = lTitle)
# #  AS and ES put option values and boundaries
# MaxValue = max(ESPutValue, ESPutLB, ASPutValue, ASPutLB)
# MinValue = min(ESPutValue, ESPutLB, ASPutValue, ASPutLB)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("ES Put", "ES LB Put", "AS Put", "AS LB Put")
# mTitle = "Put Option Values and Lower Bounds"
# xTitle = "Stock Price"
# yTitle = "Values"
# lTitle = "Parameter"
# plot(TStockPrice, ESPutValue, type = "l", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ESPutLB, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TStockPrice, ASPutValue, type = "l", col ="green", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# lines(TStockPrice, ASPutLB, type = "l", col ="purple", xlim = xlim1,
#   ylim = ylim1, pch = 4, cex = 0.5)
# legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1, 1),
#   lty = c(1, 1, 1, 1), col = c("blue", "red", "green", "purple"),
#   pch = c(NA,NA,NA), bty = "n", title = lTitle)
# # Plots: American-style Options with respect to the Stock Price w boundaries
# #  Call with boundaries
# MaxValue = max(ASCallValue, ASCallLB, ASCallUB)
# MinValue = min(ASCallValue, ASCallLB, ASCallUB)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Call Value", "Call Lower Bound", "Call Upper Bound")
# mTitle = "American-Style Binomial Option Value (ABM)"
# xTitle = "Stock Price"
# yTitle = "Call Value"
# lTitle = "Parameter"
# plot(TStockPrice, ASCallValue, type = "l", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASCallLB, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TStockPrice, ASCallUB, type = "l", col ="green", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# # Put with boundaries
# MaxValue = max(ASPutValue, ASPutLB, ASPutUB)
# MinValue = min(ASPutValue, ASPutLB, ASPutUB)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Put Value", "Put Lower Bound", "Put Upper Bound")
# mTitle = "American-Style Binomial Option Value (ABM)"
# xTitle = "Stock Price"
# yTitle = "Put Value"
# lTitle = "Parameter"
# plot(TStockPrice, ASPutValue, type = "l", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASPutLB, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TStockPrice, ASPutUB, type = "l", col ="green", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("right", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# # Calls and puts
# MaxValue = max(ASCallValue, ASPutValue)
# MinValue = min(ASCallValue, ASPutValue)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# MaxValue = max(TStockPrice); MinValue = min(TStockPrice)
# xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
# legtxt = c("Call Value","Put Value")
# mTitle = "American-Style Binomial Option Value (ABM)"
# xTitle = "Stock Price"
# yTitle = "Option Value"
# lTitle = "Parameter"
# plot(TStockPrice, ASCallValue, type = "l", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASPutValue, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("blue","red"), pch = c(NA,NA), bty = "n", title = lTitle)
# #
# #  Call Deltas (Direct only)
# #
# MaxValue = max(ESCallDeltaDirect, ASCallDeltaDirect)
# MinValue = min(ESCallDeltaDirect, ASCallDeltaDirect)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("European-Style", "American-Style")
# mTitle = "Binomial Call Option Deltas (ABM, Direct Method)"
# xTitle = "Stock Price"
# yTitle = "Delta"
# lTitle = "Parameter"
# plot(TStockPrice, ESCallDeltaDirect, type = "l", main = mTitle,
#   sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASCallDeltaDirect, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("blue", "red"), pch = c(NA, NA), bty = "n", title = lTitle)
# #
# #  Put Deltas (Direct only)
# #
# MaxValue = max(ESPutDeltaDirect, ASPutDeltaDirect)
# MinValue = min(ESPutDeltaDirect, ASPutDeltaDirect)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("European-Style", "American-Style")
# mTitle = "Binomial Put Option Deltas (ABM, Direct Method)"
# xTitle = "Stock Price"
# yTitle = "Delta"
# lTitle = "Parameter"
# plot(TStockPrice, ESPutDeltaDirect, type = "l", main = mTitle,
#   sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASPutDeltaDirect, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("blue", "red"), pch = c(NA, NA), bty = "n", title = lTitle)
# #
# #  Call Deltas
# #
# MaxValue = max(ASCallDeltaDirect, ASCallDeltaDirectEnh, ASCallDeltaNG)
# MinValue = min(ASCallDeltaDirect, ASCallDeltaDirectEnh, ASCallDeltaNG)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Direct", "Direct Enhanced", "Numerical")
# mTitle = "American-Style Binomial Call Option Deltas (ABM)"
# xTitle = "Stock Price"
# yTitle = "Delta"
# lTitle = "Parameter"
# plot(TStockPrice, ASCallDeltaDirect, type = "l", main = mTitle,
#   sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASCallDeltaDirectEnh, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TStockPrice, ASCallDeltaNG, type = "l", col ="green", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #
# #  Put Deltas
# #
# MaxValue = max(ASPutDeltaDirect, ASPutDeltaDirectEnh, ASPutDeltaNG)
# MinValue = min(ASPutDeltaDirect, ASPutDeltaDirectEnh, ASPutDeltaNG)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Direct", "Direct Enhanced", "Numerical")
# mTitle = "American-Style Binomial Put Option Deltas (ABM)"
# xTitle = "Stock Price"
# yTitle = "Delta"
# lTitle = "Parameter"
# plot(TStockPrice, ASPutDeltaDirect, type = "l", main = mTitle,
#   sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASPutDeltaDirectEnh, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TStockPrice, ASPutDeltaNG, type = "l", col ="green", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #
# #  Call Gammas
# #
# MaxValue = max(ASCallGammaDirect, ASCallGammaDirectEnh, ASCallGammaNG)
# MinValue = min(ASCallGammaDirect, ASCallGammaDirectEnh, ASCallGammaNG)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Direct", "Direct Enhanced", "Numerical")
# mTitle = "American-Style Binomial Call Option Gammas (ABM)"
# xTitle = "Stock Price"
# yTitle = "Gamma"
# lTitle = "Parameter"
# plot(TStockPrice, ASCallGammaDirect, type = "l", main = mTitle,
#   sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASCallGammaDirectEnh, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TStockPrice, ASCallGammaNG, type = "l", col ="green", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #
# #  Put Gammas
# #
# MaxValue = max(ASPutGammaDirect, ASPutGammaDirectEnh, ASPutGammaNG)
# MinValue = min(ASPutGammaDirect, ASPutGammaDirectEnh, ASPutGammaNG)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Direct", "Direct Enhanced", "Numerical")
# mTitle = "American-Style Binomial Put Option Gammas (ABM)"
# xTitle = "Stock Price"
# yTitle = "Gamma"
# lTitle = "Parameter"
# plot(TStockPrice, ASPutGammaDirect, type = "l", main = mTitle,
#   sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASPutGammaDirectEnh, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TStockPrice, ASPutGammaNG, type = "l", col ="green", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #
# #  Call Thetas
# #
# MaxValue = max(ASCallThetaDirect, ASCallThetaDirectEnh, ASCallThetaNG)
# MinValue = min(ASCallThetaDirect, ASCallThetaDirectEnh, ASCallThetaNG)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Direct", "Direct Enhanced", "Numerical")
# mTitle = "American-Style Binomial Call Option Thetas (ABM)"
# xTitle = "Stock Price"
# yTitle = "Theta"
# lTitle = "Parameter"
# plot(TStockPrice, ASCallThetaDirect, type = "l", main = mTitle,
#   sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASCallThetaDirectEnh, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TStockPrice, ASCallThetaNG, type = "l", col ="green", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #
# #  Put Thetas
# #
# MaxValue = max(ASPutThetaDirect, ASPutThetaDirectEnh, ASPutThetaNG)
# MinValue = min(ASPutThetaDirect, ASPutThetaDirectEnh, ASPutThetaNG)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Direct", "Direct Enhanced", "Numerical")
# mTitle = "American-Style Binomial Put Option Thetas (ABM)"
# xTitle = "Stock Price"
# yTitle = "Theta"
# lTitle = "Parameter"
# plot(TStockPrice, ASPutThetaDirect, type = "l", main = mTitle,
#   sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASPutThetaDirectEnh, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(TStockPrice, ASPutThetaNG, type = "l", col ="green", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("blue", "red", "green"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #
# #  Vegas
# #
# MaxValue = max(ASCallVegaNG, ASPutVegaNG)
# MinValue = min(ASCallVegaNG, ASPutVegaNG)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Call", "Put")
# mTitle = "American-Style Binomial Option Vegas (ABM)"
# xTitle = "Stock Price"
# yTitle = "Vega"
# lTitle = "Parameter"
# plot(TStockPrice, ASCallVegaNG, type = "l", main = mTitle,
#   sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASPutVegaNG, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("topright", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("blue", "red"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# #
# #  Rho
# #
# MaxValue = max(ASCallRhoNG, ASPutRhoNG)
# MinValue = min(ASCallRhoNG, ASPutRhoNG)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Call", "Put")
# mTitle = "American-Style Binomial Option Rhos (ABM)"
# xTitle = "Stock Price"
# yTitle = "Rho"
# lTitle = "Parameter"
# plot(TStockPrice, ASCallRhoNG, type = "l", main = mTitle,
#   sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, ASPutRhoNG, type = "l", col ="red", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("blue", "red"), pch = c(NA,NA,NA), bty = "n",
#   title = lTitle)
# 


# # Plain vanilla option values with respect to the number of steps
# xlim1 = c(1:2); xlim1[1] = MinStep; xlim1[2] = MaxStep
# MaxOptionValue = max(ASCallValue,ASPutValue)
# MinOptionValue = min(ASCallValue,ASPutValue)
# ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
# legtxt = c("Call Value","Put Value")
# mTitle = "European-Style Binomial Plain Vanilla Option Value (ABM)"
# xTitle = "Number of Steps"
# yTitle = "Plain Vanilla Option Value"
# lTitle = "Parameter"
# plot(NumberOfSteps, ASPutValue, type="l",
#   main=mTitle, sub=sTitleBIN,
#   xlab="Number Of Steps", ylab="Option Values", col="blue",
#   xlim = xlim1, ylim = ylim1)
# lines(NumberOfSteps, ASCallValue, col="red", xlim = xlim1, ylim = ylim1)
# legend("right", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("blue","red"), pch = c(NA,NA), bty = "n", title = lTitle)
# # Digital option values with respect to the number of steps
# MaxOptionValue = max(DigitalCallValue, DigitalPutValue)
# MinOptionValue = min(DigitalCallValue, DigitalPutValue)
# ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
# legtxt = c("Call Value","Put Value")
# mTitle = "European-Style Binomial Digital Option Value (ABM)"
# xTitle = "Stock Price"
# yTitle = "Digital Option Value"
# lTitle = "Parameter"
# plot(NumberOfSteps, DigitalCallValue, type="l",
#   main=mTitle, sub=sTitleBINDP,
#   xlab="Number Of Steps", ylab="Digital Option Values",
#   col="blue", xlim = xlim1, ylim = ylim1)
# lines(NumberOfSteps, DigitalPutValue, col="red", xlim = xlim1, ylim = ylim1)
# legend("topright", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("blue","red"), pch = c(NA,NA), bty = "n", title = lTitle)
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
# plot(TStockPrice, DigitalCallValue, type = "l", main = mTitle,
#      sub = sTitleBINDP, xlab = xTitle, ylab = yTitle, col = "blue", xlim = xlim1,
#      ylim = ylim1, pch = 1, cex = 0.5)
# lines(TStockPrice, DigitalPutValue, type = "l", col ="red", xlim = xlim1,
#       ylim = ylim1, pch = 2, cex = 0.5)
# legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#        col = c("blue","red"), pch = c(NA,NA), bty = "n", title = lTitle)


