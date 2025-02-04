# CAA Process Generic Data.R 

#
# Data:
#   Various dates
#   3-M Libor (RSpot)
#   Underlying spot (CSpot)
#   Futures contract maturities (Mati)
#   Futures prices (Ci)
#   Percentage change in futures prices (PCFi = log[C(i,t)/C(i,t-1)]
#   First difference in futures prices FDi = C(i,t) - C(i,t-1)
#   First difference in underlying CSpotFD = CSpot(t) - CSpot(t-1)
#   Futures contract maturity month (Di = YEARMM)
#   Futures contract volume (Vi)
#   Futures contract open interest (Oi)
#   Dollar spread SPRi = C(i,t) - C(i-1,t)
#   Implied carry cost CCi = log[C(i,t)/C(i-1,t) or CSpot] (annualized?)
#   Si ?
#   CSPRi ?
#   Pseudo contract maturitiess (PsMati = i/12)
#   Pseudo futures price (PsCi, based on LSC model applied to futures prices)
#   Pseudo term preimium (PsTPi)
#   Pseudo percentage change in futures prices(PsPCFi) based on LSC futures prices
#   Pseudo marginal contribution (PsMCi)
#
# Arb data in Temp:
#   TPi = log[Ci/C(i-1)]  (1 is to spot, use 2, 3) 
#   MCi = log[Ci,t/C(i,t-1)] - log[C(i-1,t)/C(i-1,t-1)] (use 2, 3)
#
# Rolling statistical parameters (EWMA)
#
Weights = as.numeric(c(1:RollingWindow)) # Exponentially weighted moving average
Sum = 0.0
for(i in 1:RollingWindow){
  Weights[i] <- (1 - Lambda)*Lambda^i
  Sum = Sum + Weights[i]
}
Weights <- Weights/Sum
Weights <- sort(Weights, decreasing = FALSE) # Apply heaviest weight to recent obs
SumW <- sum(Weights)
mTitle <- paste0(mTitle1, '\nFutures Percentage Change')
source('Analysis of PCF.R')
mTitle <- paste0(mTitle1, '\nFutures First Difference')
source('Analysis of FD.R')
