# LSC Model Greek Functions.R

source("Traditional Valuation Functions.R")
source("LSC Model Functions.R")
#
# Numerical Greeks
#
#
# Numeric Greeks
#
# Traditional model, g5 (stage 1, first 5 years)
TradDeltag5 = function(TG){
  with(TG, {
    Increment <- 0.0001
    Original <- g1
    Change <- Increment*Original
    High <- Original + Change
    TG$g1 <- High
    OHigh <- TraditionalInstrumentValue(TG)
    Low <- Original - Change
    TG$g1 <- Low
    OLow <- TraditionalInstrumentValue(TG)
    TG$g1 <- Original
    Answer <- (OHigh - OLow)/(High - Low)
    return(Answer)
  })  
}
# Perpetual growth
TradDeltag5P = function(TG){
  with(TG, {
    Increment <- 0.0001
    Original <- g2
    Change <- Increment*Original
    if(Change == 0.0)Change <- 0.0001
    High <- Original + Change
    TG$g2 <- High
    OHigh <- TraditionalInstrumentValue(TG)
    Low <- Original - Change
    TG$g2 <- Low
    OLow <- TraditionalInstrumentValue(TG)
    TG$g2 <- Original
    Answer <- (OHigh - OLow)/(High - Low)
    return(Answer)
  })  
}
# WACC
TradDeltaWACC = function(TG){
  with(TG, {
    Increment <- 0.0001
    Original <- WACC
    Change <- Increment*Original
    High <- Original + Change
    TG$WACC <- High
    OHigh <- TraditionalInstrumentValue(TG)
    Low <- Original - Change
    TG$WACC <- Low
    OLow <- TraditionalInstrumentValue(TG)
    TG$WACC <- Original
    Answer <- (OHigh - OLow)/(High - Low)
    return(Answer)
  })  
}


