# Carry Arbitrage Analysis.R
# LoadTickers <- TRUE
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
Arb = data.frame(matrix(vector(), LengthArb, 5, dimnames=list(c(),
  c("Commodity", "Arb1Average", "Arb1StdDev", "Arb2Average", "Arb2StdDev"
  ))), stringsAsFactors=F)


iij = 1
Commodity <- 'USDSPX'
File <- 'SPX LSC.xlsx'
mTitle1 <- 'US SPX (SP)'
source('CAA Process Generic Data.R')
Arb$Commodity[iij] <- Commodity
Arb$Arb1Average[iij] <- MDataAverage1
Arb$Arb1StdDev[iij] <- MDataStdDev1
Arb$Arb2Average[iij] <- MDataAverage2
Arb$Arb2StdDev[iij] <- MDataStdDev2





# #
# # Gold2 LSC test case (GC2)
# #
# iij = 1
# Commodity <- 'USDGold'
# File <- 'Gold2 LSC.xlsx'
# mTitle1 <- 'US Gold (GC2)'
# source('CAA Process Generic Data.R')
# Arb$Commodity[iij] <- Commodity
# Arb$Arb1Average[iij] <- MDataAverage1
# Arb$Arb1StdDev[iij] <- MDataStdDev1
# Arb$Arb2Average[iij] <- MDataAverage2
# Arb$Arb2StdDev[iij] <- MDataStdDev2
# iij <- iij + 1
# Commodity<- 'CNYGold'
# File <- 'CNYGold LSC.xlsx'
# mTitle1 <- 'China Gold (SHG)'
# source('CAA Process Generic Data.R')
# Arb$Commodity[iij] <- Commodity
# Arb$Arb1Average[iij] <- MDataAverage1
# Arb$Arb1StdDev[iij] <- MDataStdDev1
# Arb$Arb2Average[iij] <- MDataAverage2
# Arb$Arb2StdDev[iij] <- MDataStdDev2
# head(Arb, 2)


# iij <- iij + 1
# Commodity <- 'USDUST10'
# File <- 'UST10 LSC.xlsx'
# mTitle1 <- 'US 10Y TBond (TY_)'
# source('CAA Process Generic Data.R')
# Arb$Commodity[iij] <- Commodity
# Arb$Arb1Average[iij] <- MDataAverage1
# Arb$Arb1StdDev[iij] <- MDataStdDev1
# Arb$Arb2Average[iij] <- MDataAverage2
# Arb$Arb2StdDev[iij] <- MDataStdDev2
# iij <- iij + 1
# Commodity<- 'CNY10YRTBond'
# File <- 'CNY10YRTBond LSC.xlsx'
# mTitle1 <- 'China 10Y TBond (CTT)'
# source('CAA Process Generic Data.R')
# Arb$Commodity[iij] <- Commodity
# Arb$Arb1Average[iij] <- MDataAverage1
# Arb$Arb1StdDev[iij] <- MDataStdDev1
# Arb$Arb2Average[iij] <- MDataAverage2
# Arb$Arb2StdDev[iij] <- MDataStdDev2
# iij <- iij + 1
# head(Arb, 5)
