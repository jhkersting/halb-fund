# 10.1 FCA SRM Test.R
# Futures Carry Arbitrage Static Risk Measuresw
# rmarkdown::render("10.1 FCA SRM Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# FCA inputs
inputIndexPrice = 100       
inputInterestRate = 5.0        # In percent
inputDividendYield = 2.0       # In percent
inputTimeToMaturity = 1.0
#
# FCA list
#
FCAInputData <- list(inputIndexPrice, inputInterestRate, inputDividendYield, 
  inputTimeToMaturity)
names(FCAInputData) <- c("IndexPrice", "InterestRate", "DividendYield", 
  "TimeToMaturity")
source("FCAVM With SRM Functions.R")
UIValue = FCAInputData$IndexPrice
TestFVwDAI = FVwDAI(inputTimeToMaturity, inputInterestRate, inputDividendYield)
TestFVwDA = FVwDA(FCAInputData)
TestFVwDAI; TestFVwDA
TestFVCA <- FVCA(FCAInputData)
TestFVCA
TestFVCADelta <- FVCADelta(FCAInputData)
TestFVCADelta
TestFVCAGamma <- FVCAGamma(FCAInputData)
TestFVCAGamma
TestFVCATheta <- FVCATheta(FCAInputData)
TestFVCATheta
TestFVCAVega <- FVCAVega(FCAInputData)
TestFVCAVega
TestFVCARho <- FVCARho(FCAInputData)
TestFVCARho
