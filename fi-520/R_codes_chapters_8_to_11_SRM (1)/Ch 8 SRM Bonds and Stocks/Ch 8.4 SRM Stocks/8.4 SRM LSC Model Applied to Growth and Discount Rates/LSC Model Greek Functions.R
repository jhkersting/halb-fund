# LSC Model Greek Functions.R

source("LSC Model Functions.R")
#
# Numerical Greeks
#
# Numerical Greek (Lg)
LSCNDeltaLg = function(L){
  with(L, {
    Increment <- 0.0000001
    Original <- LSCGLevel
    Change <- Increment*Original
    High <- Original + Change
    L$LSCGLevel <- High
    OHigh <- LSCInstrumentValueGDR(L)
    Low <- Original - Change
    L$LSCGLevel <- Low
    OLow <- LSCInstrumentValueGDR(L)
    L$LSCGLevel <- Original
    Answer <- (OHigh - OLow)/(High - Low)
    return(Answer)
  })  
}
# Numerical Greek (Lf)
LSCNDeltaLf = function(L){
  with(L, {
    Increment <- 0.0000001
    Original <- LSCDRLevel
    Change <- Increment*Original
    High <- Original + Change
    L$LSCDRLevel <- High
    OHigh <- LSCInstrumentValueGDR(L)
    Low <- Original - Change
    L$LSCDRLevel <- Low
    OLow <- LSCInstrumentValueGDR(L)
    L$LSCDRLevel <- Original
    Answer <- (OHigh - OLow)/(High - Low)
    return(Answer)
  })  
}
# Numerical Greek (Lg)
LSCNDeltaSg = function(L){
  with(L, {
    Increment <- 0.0000001
    Original <- LSCGSlope
    Change <- Increment*Original
    High <- Original + Change
    L$LSCGSlope <- High
    OHigh <- LSCInstrumentValueGDR(L)
    Low <- Original - Change
    L$LSCGSlope <- Low
    OLow <- LSCInstrumentValueGDR(L)
    L$LSCGSlope <- Original
    Answer <- (OHigh - OLow)/(High - Low)
    return(Answer)
  })  
}
# Numerical Greek (Lf)
LSCNDeltaSf = function(L){
  with(L, {
    Increment <- 0.0000001
    Original <- LSCDRSlope
    Change <- Increment*Original
    High <- Original + Change
    L$LSCDRSlope <- High
    OHigh <- LSCInstrumentValueGDR(L)
    Low <- Original - Change
    L$LSCDRSlope <- Low
    OLow <- LSCInstrumentValueGDR(L)
    L$LSCDRSlope <- Original
    Answer <- (OHigh - OLow)/(High - Low)
    return(Answer)
  })  
}
# LSC model Delta Lg
LSCDeltaLg = function(L){
  with(L, {
    Mat <- c(1:LSCNumberOfYears)
    CF <- c(1:LSCNumberOfYears)
    GR <- c(1:LSCNumberOfYears)
    DR <- c(1:LSCNumberOfYears)
    DF <- c(1:LSCNumberOfYears)
    PVCF <- c(1:LSCNumberOfYears)
    Sumscg <- 0.0
    for (j in 1:LSCNumberOfYears){
      GR[j] <- LSCGLevel +
        LSCGSlope * ((1.0 - exp(-Mat[j]/LSCScalarG))/(Mat[j]/LSCScalarG)) +
        LSCGCurve1 * ((1.0-exp(-Mat[j]/LSCScalarG)) / 
          (Mat[j]/LSCScalarG) - exp(-Mat[j]/LSCScalarG))
      DR[j] <- LSCDRLevel +
        LSCDRSlope * ((1.0 - exp(-Mat[j]/LSCScalarDR))/(Mat[j]/LSCScalarDR)) +
        LSCDRCurve1 * ((1.0-exp(-Mat[j]/LSCScalarDR)) / 
          (Mat[j]/LSCScalarDR) - exp(-Mat[j]/LSCScalarDR))
      if(j == 1){
        CF[j] <- LSCInitialCF * exp(GR[j])
        DF[j] <- exp(-DR[j])
        PVCF[j] <- j * CF[j] * DF[j]
      }
      if(j > 1){
        CF[j] <- CF[j-1] * exp(GR[j])
        DF[j] <- DF[j-1] * exp(-DR[j])
        PVCF[j] <- j * CF[j] * DF[j]
      }
    }
    InstrumentGreek <- sum(PVCF)
    return( InstrumentGreek )
  })
}
# LSC model Delta Lf
LSCDeltaLf = function(L){
  with(L, {
    Mat <- c(1:LSCNumberOfYears)
    CF <- c(1:LSCNumberOfYears)
    GR <- c(1:LSCNumberOfYears)
    DR <- c(1:LSCNumberOfYears)
    DF <- c(1:LSCNumberOfYears)
    PVCF <- c(1:LSCNumberOfYears)
    for (j in 1:LSCNumberOfYears){
      GR[j] <- LSCGLevel +
        LSCGSlope * ((1.0 - exp(-Mat[j]/LSCScalarG))/(Mat[j]/LSCScalarG)) +
        LSCGCurve1 * ((1.0-exp(-Mat[j]/LSCScalarG)) / 
          (Mat[j]/LSCScalarG) - exp(-Mat[j]/LSCScalarG))
      DR[j] <- LSCDRLevel +
        LSCDRSlope * ((1.0 - exp(-Mat[j]/LSCScalarDR))/(Mat[j]/LSCScalarDR)) +
        LSCDRCurve1 * ((1.0-exp(-Mat[j]/LSCScalarDR)) / 
          (Mat[j]/LSCScalarDR) - exp(-Mat[j]/LSCScalarDR))
      if(j == 1){
        CF[j] <- LSCInitialCF * exp(GR[j])
        DF[j] <- exp(-DR[j])
        PVCF[j] <- j * CF[j] * DF[j]
      }
      if(j > 1){
        CF[j] <- CF[j-1] * exp(GR[j])
        DF[j] <- DF[j-1] * exp(-DR[j])
        PVCF[j] <- j * CF[j] * DF[j]
      }
    }
    InstrumentGreek <- sum(PVCF)
    return( -InstrumentGreek )
  })
}
# LSC model Delta Sg
LSCDeltaSg = function(L){
  with(L, {
    Mat <- c(1:LSCNumberOfYears)
    CF <- c(1:LSCNumberOfYears)
    GR <- c(1:LSCNumberOfYears)
    DR <- c(1:LSCNumberOfYears)
    DF <- c(1:LSCNumberOfYears)
    PVCF <- c(1:LSCNumberOfYears)
    Sumscg <- c(1:LSCNumberOfYears)
    for (j in 1:LSCNumberOfYears){
      Sumscg[j] <- 0.0
      for(k in 1:j){
        Sumscg[j] <- Sumscg[j] + ((1.0 - exp(-Mat[k]/LSCScalarG))/(Mat[k]/LSCScalarG))
      }
      GR[j] <- LSCGLevel +
        LSCGSlope * ((1.0 - exp(-Mat[j]/LSCScalarG))/(Mat[j]/LSCScalarG)) +
        LSCGCurve1 * ((1.0-exp(-Mat[j]/LSCScalarG)) / 
          (Mat[j]/LSCScalarG) - exp(-Mat[j]/LSCScalarG))
      DR[j] <- LSCDRLevel +
        LSCDRSlope * ((1.0 - exp(-Mat[j]/LSCScalarDR))/(Mat[j]/LSCScalarDR)) +
        LSCDRCurve1 * ((1.0-exp(-Mat[j]/LSCScalarDR)) / 
          (Mat[j]/LSCScalarDR) - exp(-Mat[j]/LSCScalarDR))
      if(j == 1){
        CF[j] <- LSCInitialCF * exp(GR[j])
        DF[j] <- exp(-DR[j])
        PVCF[j] <- Sumscg[j] * CF[j] * DF[j]
      }
      if(j > 1){
        CF[j] <- CF[j-1] * exp(GR[j])
        DF[j] <- DF[j-1] * exp(-DR[j])
        PVCF[j] <- Sumscg[j] * CF[j] * DF[j]
      }
    }
    InstrumentGreek <- sum(PVCF)
    return( InstrumentGreek )
  })
}
# LSC model Delta Sf
LSCDeltaSf = function(L){
  with(L, {
    Mat <- c(1:LSCNumberOfYears)
    CF <- c(1:LSCNumberOfYears)
    GR <- c(1:LSCNumberOfYears)
    DR <- c(1:LSCNumberOfYears)
    DF <- c(1:LSCNumberOfYears)
    PVCF <- c(1:LSCNumberOfYears)
    Sumscf <- c(1:LSCNumberOfYears)
    for (j in 1:LSCNumberOfYears){
      Sumscf[j] <- 0.0
      for(k in 1:j){
        Sumscf[j] <- Sumscf[j] + ((1.0 - exp(-Mat[k]/LSCScalarDR))/(Mat[k]/LSCScalarDR))
      }
      GR[j] <- LSCGLevel +
        LSCGSlope * ((1.0 - exp(-Mat[j]/LSCScalarG))/(Mat[j]/LSCScalarG)) +
        LSCGCurve1 * ((1.0-exp(-Mat[j]/LSCScalarG)) / 
          (Mat[j]/LSCScalarG) - exp(-Mat[j]/LSCScalarG))
      DR[j] <- LSCDRLevel +
        LSCDRSlope * ((1.0 - exp(-Mat[j]/LSCScalarDR))/(Mat[j]/LSCScalarDR)) +
        LSCDRCurve1 * ((1.0-exp(-Mat[j]/LSCScalarDR)) / 
          (Mat[j]/LSCScalarDR) - exp(-Mat[j]/LSCScalarDR))
      if(j == 1){
        CF[j] <- LSCInitialCF * exp(GR[j])
        DF[j] <- exp(-DR[j])
        PVCF[j] <- Sumscf[j] * CF[j] * DF[j]
      }
      if(j > 1){
        CF[j] <- CF[j-1] * exp(GR[j])
        DF[j] <- DF[j-1] * exp(-DR[j])
        PVCF[j] <- Sumscf[j] * CF[j] * DF[j]
      }
    }
    InstrumentGreek <- sum(PVCF)
    return( -InstrumentGreek )
  })
}

