# SRM ABM Binomial OVM Comparison Plots.R
# # Plots: Selected Comparison
MaxValue = max(TStockPrice); MinValue = min(TStockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
#
#  Numerical Deltas - ES v AS, Calls vs. Puts
#
MaxValue = max(ESCallDeltaNG, ASCallDeltaNG, ESPutDeltaNG, ASPutDeltaNG)
MinValue = min(ESCallDeltaNG, ASCallDeltaNG, ESPutDeltaNG, ASPutDeltaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call", "AS Call", "ES Put", "AS Put")
mTitle = "Call Option Deltas (ABM, Numerical Method)"
xTitle = "Stock Price"
yTitle = "Delta"
lTitle = "Parameter"
plot(TStockPrice, ESCallDeltaNG, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallDeltaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutDeltaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(TStockPrice, ASPutDeltaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0),
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n",
  title = lTitle)
#
#  Direct (Standard) Deltas - ES v AS, Calls vs. Puts
#
MaxValue = max(ESCallDeltaDirect, ASCallDeltaDirect,
  ESPutDeltaDirect, ASPutDeltaDirect)
MinValue = min(ESCallDeltaDirect, ASCallDeltaDirect,
  ESPutDeltaDirect, ASPutDeltaDirect)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call", "AS Call", "ES Put", "AS Put")
mTitle = "Call Option Deltas (ABM, Standard Method)"
xTitle = "Stock Price"
yTitle = "Delta"
lTitle = "Parameter"
plot(TStockPrice, ESCallDeltaDirect, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallDeltaDirect, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutDeltaDirect, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(TStockPrice, ASPutDeltaDirect, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0),
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n",
  title = lTitle)
#
#  Direct Enhanced (Standard) Deltas - ES v AS, Calls vs. Puts
#
MaxValue = max(ESCallDeltaDirectEnh, ASCallDeltaDirectEnh,
  ESPutDeltaDirectEnh, ASPutDeltaDirectEnh)
MinValue = min(ESCallDeltaDirectEnh, ASCallDeltaDirectEnh,
  ESPutDeltaDirectEnh, ASPutDeltaDirectEnh)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call", "AS Call", "ES Put", "AS Put")
mTitle = "Call Option Deltas (ABM, Enhanced Method)"
xTitle = "Stock Price"
yTitle = "Delta"
lTitle = "Parameter"
plot(TStockPrice, ESCallDeltaDirectEnh, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallDeltaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutDeltaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(TStockPrice, ASPutDeltaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0),
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n",
  title = lTitle)
#
#  Numerical Gammas - ES v AS, Calls vs. Puts
#
MaxValue = max(ESCallGammaNG, ASCallGammaNG, ESPutGammaNG, ASPutGammaNG)
MinValue = min(ESCallGammaNG, ASCallGammaNG, ESPutGammaNG, ASPutGammaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call", "AS Call", "ES Put", "AS Put")
mTitle = "Call Option Gammas (ABM, Numerical Method)"
xTitle = "Stock Price"
yTitle = "Gamma"
lTitle = "Parameter"
plot(TStockPrice, ESCallGammaNG, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallGammaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutGammaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(TStockPrice, ASPutGammaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0),
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n",
  title = lTitle)
#
#  Direct (Standard) Gammas - ES v AS, Calls vs. Puts
#
MaxValue = max(ESCallGammaDirect, ASCallGammaDirect,
  ESPutGammaDirect, ASPutGammaDirect)
MinValue = min(ESCallGammaDirect, ASCallGammaDirect,
  ESPutGammaDirect, ASPutGammaDirect)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call", "AS Call", "ES Put", "AS Put")
mTitle = "Call Option Gammas (ABM, Standard Method)"
xTitle = "Stock Price"
yTitle = "Gamma"
lTitle = "Parameter"
plot(TStockPrice, ESCallGammaDirect, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallGammaDirect, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutGammaDirect, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(TStockPrice, ASPutGammaDirect, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0),
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n",
  title = lTitle)
#
#  Direct Enhanced (Standard) Gammas - ES v AS, Calls vs. Puts
#
MaxValue = max(ESCallGammaDirectEnh, ASCallGammaDirectEnh,
  ESPutGammaDirectEnh, ASPutGammaDirectEnh)
MinValue = min(ESCallGammaDirectEnh, ASCallGammaDirectEnh,
  ESPutGammaDirectEnh, ASPutGammaDirectEnh)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call", "AS Call", "ES Put", "AS Put")
mTitle = "Call Option Gammas (ABM, Enhanced Method)"
xTitle = "Stock Price"
yTitle = "Gamma"
lTitle = "Parameter"
plot(TStockPrice, ESCallGammaDirectEnh, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallGammaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutGammaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(TStockPrice, ASPutGammaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0),
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n",
  title = lTitle)
#
#  Numerical Thetas - ES v AS, Calls vs. Puts
#
MaxValue = max(ESCallThetaNG, ASCallThetaNG, ESPutThetaNG, ASPutThetaNG)
MinValue = min(ESCallThetaNG, ASCallThetaNG, ESPutThetaNG, ASPutThetaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call", "AS Call", "ES Put", "AS Put")
mTitle = "Call Option Thetas (ABM, Numerical Method)"
xTitle = "Stock Price"
yTitle = "Theta"
lTitle = "Parameter"
plot(TStockPrice, ESCallThetaNG, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallThetaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutThetaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(TStockPrice, ASPutThetaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0),
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n",
  title = lTitle)
#
#  Direct (Standard) Thetas - ES v AS, Calls vs. Puts
#
MaxValue = max(ESCallThetaDirect, ASCallThetaDirect, 
  ESPutThetaDirect, ASPutThetaDirect)
MinValue = min(ESCallThetaDirect, ASCallThetaDirect, 
  ESPutThetaDirect, ASPutThetaDirect)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call", "AS Call", "ES Put", "AS Put")
mTitle = "Call Option Thetas (ABM, Standard Method)"
xTitle = "Stock Price"
yTitle = "Theta"
lTitle = "Parameter"
plot(TStockPrice, ESCallThetaDirect, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallThetaDirect, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutThetaDirect, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(TStockPrice, ASPutThetaDirect, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0),
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n",
  title = lTitle)
#
#  Direct Enhanced (Standard) Thetas - ES v AS, Calls vs. Puts
#
MaxValue = max(ESCallThetaDirectEnh, ASCallThetaDirectEnh, 
  ESPutThetaDirectEnh, ASPutThetaDirectEnh)
MinValue = min(ESCallThetaDirectEnh, ASCallThetaDirectEnh, 
  ESPutThetaDirectEnh, ASPutThetaDirectEnh)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call", "AS Call", "ES Put", "AS Put")
mTitle = "Call Option Thetas (ABM, Enhanced Method)"
xTitle = "Stock Price"
yTitle = "Theta"
lTitle = "Parameter"
plot(TStockPrice, ESCallThetaDirectEnh, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallThetaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutThetaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(TStockPrice, ASPutThetaDirectEnh, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0),
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n",
  title = lTitle)
#
#  Numerical Vegas - ES v AS, Calls vs. Puts
#
MaxValue = max(ESCallVegaNG, ASCallVegaNG, ESPutVegaNG, ASPutVegaNG)
MinValue = min(ESCallVegaNG, ASCallVegaNG, ESPutVegaNG, ASPutVegaNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call", "AS Call", "ES Put", "AS Put")
mTitle = "Call Option Vegas (ABM, Numerical Method)"
xTitle = "Stock Price"
yTitle = "Vega"
lTitle = "Parameter"
plot(TStockPrice, ESCallVegaNG, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallVegaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutVegaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(TStockPrice, ASPutVegaNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0),
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n",
  title = lTitle)
#
#  Numerical Rhos - ES v AS, Calls vs. Puts
#
MaxValue = max(ESCallRhoNG, ASCallRhoNG, ESPutRhoNG, ASPutRhoNG)
MinValue = min(ESCallRhoNG, ASCallRhoNG, ESPutRhoNG, ASPutRhoNG)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call", "AS Call", "ES Put", "AS Put")
mTitle = "Call Option Rhos (ABM, Numerical Method)"
xTitle = "Stock Price"
yTitle = "Rho"
lTitle = "Parameter"
plot(TStockPrice, ESCallRhoNG, type = "b", main = mTitle,
  sub = sTitleBINGk, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(TStockPrice, ASCallRhoNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(TStockPrice, ESPutRhoNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(TStockPrice, ASPutRhoNG, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1), lty = c(0,0,0,0),
  col = c("black", "black", "black", "black"), pch = c(1, 2, 3, 4), bty = "n",
  title = lTitle)
