# Traditional Model Greeks.R
# NOTE: Assumes LSC model is fully calibrated and data.frame II is complete
source("Traditional Valuation Functions.R")
source("Traditional Model Greek Functions.R")
#
# Numeric Greeks
#
# Traditional model predefined:
# TraditionalModelInputs <- list(inputg1, inputg2, inputYearsInStage1, 
#   inputNumberOfYears, inputWACC, inputInitialCF, inputImpliedLowerBound, 
#   inputImpliedUpperBound)
# names(TraditionalModelInputs) <- c("g1", "g2", "YearsInStage1", 
#   "NumberOfYears", "WACC", "InitialCF", 
#   "ImpliedLowerBound", "ImpliedUpperBound")
LengthII <- length(II$PE)
for(i in 1:LengthII){
  TraditionalModelInputs$WACC <- II$WACC[i]/100.0 # Stored in II in percent
  TraditionalModelInputs$g1 <- II$g5I[i]/100.0
  TraditionalModelInputs$g2 <- II$g5PI[i]/100.0
  II$PETest[i] <- TraditionalInstrumentValue(TraditionalModelInputs)
  II$Deltag5[i] <- TradDeltag5(TraditionalModelInputs)/100 # In percent
  II$Elastg5[i] <- 100 * II$Deltag5[i] * (TraditionalModelInputs$g1 / 
    II$PETest[i])
  II$Deltag5P[i] <- TradDeltag5P(TraditionalModelInputs)/100 # In percent
  if(TraditionalModelInputs$g2 == 0){
    II$Elastg5P[i] <- 100 * II$Deltag5P[i] * 
      ((TraditionalModelInputs$g2 + 0.0001) / II$PETest[i])
  } else {
    II$Elastg5P[i] <- 100 * II$Deltag5P[i] * (TraditionalModelInputs$g2 / 
      II$PETest[i])
  }
  II$DeltaWACC[i] <- TradDeltaWACC(TraditionalModelInputs)/100 # In percent
  II$ElastWACC[i] <- 100 * II$DeltaWACC[i] * (TraditionalModelInputs$WACC / 
    II$PETest[i])
}
