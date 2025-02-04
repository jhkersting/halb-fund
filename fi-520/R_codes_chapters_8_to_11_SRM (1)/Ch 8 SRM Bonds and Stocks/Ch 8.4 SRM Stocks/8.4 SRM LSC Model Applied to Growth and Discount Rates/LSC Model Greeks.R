# LSC Model Greeks.R
# NOTE: Assumes LSC model is fully calibrated and data.frame II is complete
# source("LSC Model Calibration.R")
source("LSC Model Greek Functions.R")
#
# Numeric Greeks
#
# LSC model predefined:
# LSCModelInputs <- list(inputGLevel, inputGSlope, inputGCurve1, 
#   inputDRLevel, inputDRSlope, inputDRCurve1, 
#   inputScalarG, inputScalarDR, 
#   inputNumberOfYears, inputDR, inputInitialCF, 
#   inputImpliedLowerBound, inputImpliedUpperBound)
# names(LSCModelInputs) <- c("LSCGLevel", "LSCGSlope", "LSCGCurve1",
#   "LSCDRLevel", "LSCDRSlope", "LSCDRCurve1",
#   "LSCScalarG", "LSCScalarDR", 
#   "LSCNumberOfYears", "LSCDR", "LSCInitialCF", 
#   "ImpliedLowerBound", "ImpliedUpperBound")
LengthII <- length(II$Price)
for(i in 1:LengthII){
  LSCModelInputs$LSCGLevel <- II$GLevel[i]/100.0
  LSCModelInputs$LSCGSlope <- II$GSlope[i]/100.0
  LSCModelInputs$LSCGCurve1 <- 0
  LSCModelInputs$LSCDRLevel <- II$DRLevel[i]/100.0
  LSCModelInputs$LSCDRSlope <- II$DRSlope[i]/100.0
  LSCModelInputs$LSCDRCurve1 <- 0
  LSCModelInputs$LSCScalarG <- II$ScalarG[i]
  LSCModelInputs$LSCScalarDR <- II$ScalarDR[i]
  II$PriceTest[i] <- LSCInstrumentValueGDR(LSCModelInputs)
  II$NDeltaLg[i] <- LSCNDeltaLg(LSCModelInputs)/10000 # In basis points
  II$NElastLg[i] <- 10000 * II$NDeltaLg[i] * (LSCModelInputs$LSCGLevel / 
    II$PriceTest[i])
  II$DeltaLg[i] <- LSCDeltaLg(LSCModelInputs)/10000 # In percent
  II$NDeltaLf[i] <- LSCNDeltaLf(LSCModelInputs)/10000 # In percent
  II$NElastLf[i] <- 10000 * II$NDeltaLf[i] * (LSCModelInputs$LSCDRLevel / 
    II$PriceTest[i])
  II$DeltaLf[i] <- LSCDeltaLf(LSCModelInputs)/10000 # In percent
  II$NDeltaSg[i] <- LSCNDeltaSg(LSCModelInputs)/10000 # In percent
  II$NElastSg[i] <- 10000 * II$NDeltaSg[i] * (LSCModelInputs$LSCGSlope /
    II$PriceTest[i])
  II$DeltaSg[i] <- LSCDeltaSg(LSCModelInputs)/10000 # In percent
  II$NDeltaSf[i] <- LSCNDeltaSf(LSCModelInputs)/10000 # In percent
  II$NElastSf[i] <- 10000 * II$NDeltaSf[i] * (LSCModelInputs$LSCDRSlope /
    II$PriceTest[i])
  II$DeltaSf[i] <- LSCDeltaSf(LSCModelInputs)/10000 # In percent
# Check one delta
  LSCModelInputs$LSCGLevel <- II$GLevel[i]/100.0 + 0.0001
  II$DeltaLgTest[i] <- LSCInstrumentValueGDR(LSCModelInputs) - II$PriceTest[i]
# Deltas of PCF
  II$DeltaPCFLg[i] <- II$DeltaLg[i]/II$CF[i]
  II$DeltaPCFLf[i] <- II$DeltaLf[i]/II$CF[i]
  II$DeltaPCFSg[i] <- II$DeltaSg[i]/II$CF[i]
  II$DeltaPCFSf[i] <- II$DeltaSf[i]/II$CF[i]
}

Greeks <- II
keepCol <- c("Industry", "Ticker", "DeltaLg", "DeltaLf", "DeltaSg", "DeltaSf",
  "DeltaPCFLg", "DeltaPCFLf", "DeltaPCFSg", "DeltaPCFSf")
Greeks <- II[,keepCol] 
wb <- createWorkbook("LSC Output Static Risk Measures.xlsx")
addWorksheet(wb, "First Derivatives")
writeData(wb, "First Derivatives", Greeks)
saveWorkbook(wb, file = "LSC Output Static Risk Measures.xlsx", overwrite = TRUE)

