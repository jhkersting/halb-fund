# SRM SWAP Second Derivative Functions.R
#
# Second derivative wrt FC level (numerical)
#
NSDwrtFCL = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original <- dfSWAP$FRParm1
  Change <- Increment*Original
  if(Change == 0)Change <- 0.01
  High4 <- Original + 4.0*Change
  dfSWAP$FRParm1 <- High4
  SVHigh4 <- SwapValuettd(dfSWAP, dfHDInputs)
  High3 <- Original + 3.0*Change
  dfSWAP$FRParm1 <- High3
  SVHigh3 <- SwapValuettd(dfSWAP, dfHDInputs)
  High2 <- Original + 2.0*Change
  dfSWAP$FRParm1 <- High2
  SVHigh2 <- SwapValuettd(dfSWAP, dfHDInputs)
  High1 <- Original + 1.0*Change
  dfSWAP$FRParm1 <- High1
  SVHigh1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$FRParm1 <- Original
  SVOriginal <- SwapValuettd(dfSWAP, dfHDInputs)
  Low4 <- Original - 4.0*Change
  dfSWAP$FRParm1 <- Low4
  SVLow4 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low3 <- Original - 3.0*Change
  dfSWAP$FRParm1 <- Low3
  SVLow3 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low2 <- Original - 2.0*Change
  dfSWAP$FRParm1 <- Low2
  SVLow2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low1 <- Original - 1.0*Change
  dfSWAP$FRParm1 <- Low1
  SVLow1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$FRParm1 <- Original
  Answer <- 2*(-SVLow4/1120 + 4*SVLow3/315 - SVLow2/10 + 4*SVLow1/5 - 
    205*SVOriginal/144 +
    4*SVHigh1/5 - SVHigh2/10 + 4*SVHigh3/315 -SVHigh4/1120)/(Change^2)
  return(Answer)
}
#
# Second derivative wrt FC slope (numerical)
#
NSDwrtFCS = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original <- dfSWAP$FRParm2
  Change <- Increment*Original
  if(Change == 0)Change <- 0.01
  High4 <- Original + 4.0*Change
  dfSWAP$FRParm2 <- High4
  SVHigh4 <- SwapValuettd(dfSWAP, dfHDInputs)
  High3 <- Original + 3.0*Change
  dfSWAP$FRParm2 <- High3
  SVHigh3 <- SwapValuettd(dfSWAP, dfHDInputs)
  High2 <- Original + 2.0*Change
  dfSWAP$FRParm2 <- High2
  SVHigh2 <- SwapValuettd(dfSWAP, dfHDInputs)
  High1 <- Original + 1.0*Change
  dfSWAP$FRParm2 <- High1
  SVHigh1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$FRParm2 <- Original
  SVOriginal <- SwapValuettd(dfSWAP, dfHDInputs)
  Low4 <- Original - 4.0*Change
  dfSWAP$FRParm2 <- Low4
  SVLow4 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low3 <- Original - 3.0*Change
  dfSWAP$FRParm2 <- Low3
  SVLow3 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low2 <- Original - 2.0*Change
  dfSWAP$FRParm2 <- Low2
  SVLow2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low1 <- Original - 1.0*Change
  dfSWAP$FRParm2 <- Low1
  SVLow1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$FRParm2 <- Original
  Answer <- 2*(-SVLow4/1120 + 4*SVLow3/315 - SVLow2/10 + 4*SVLow1/5 - 
    205*SVOriginal/144 +
    4*SVHigh1/5 - SVHigh2/10 + 4*SVHigh3/315 -SVHigh4/1120)/(Change^2)
  return(Answer)
}
#
# Second derivative wrt FC curvature 1 (numerical)
#
NSDwrtFCC1 = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original <- dfSWAP$FRParm3
  Change <- Increment*Original
  if(Change == 0)Change <- 0.01
  High4 <- Original + 4.0*Change
  dfSWAP$FRParm3 <- High4
  SVHigh4 <- SwapValuettd(dfSWAP, dfHDInputs)
  High3 <- Original + 3.0*Change
  dfSWAP$FRParm3 <- High3
  SVHigh3 <- SwapValuettd(dfSWAP, dfHDInputs)
  High2 <- Original + 2.0*Change
  dfSWAP$FRParm3 <- High2
  SVHigh2 <- SwapValuettd(dfSWAP, dfHDInputs)
  High1 <- Original + 1.0*Change
  dfSWAP$FRParm3 <- High1
  SVHigh1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$FRParm3 <- Original
  SVOriginal <- SwapValuettd(dfSWAP, dfHDInputs)
  Low4 <- Original - 4.0*Change
  dfSWAP$FRParm3 <- Low4
  SVLow4 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low3 <- Original - 3.0*Change
  dfSWAP$FRParm3 <- Low3
  SVLow3 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low2 <- Original - 2.0*Change
  dfSWAP$FRParm3 <- Low2
  SVLow2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low1 <- Original - 1.0*Change
  dfSWAP$FRParm3 <- Low1
  SVLow1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$FRParm3 <- Original
  Answer <- 2*(-SVLow4/1120 + 4*SVLow3/315 - SVLow2/10 + 4*SVLow1/5 - 
    205*SVOriginal/144 +
    4*SVHigh1/5 - SVHigh2/10 + 4*SVHigh3/315 -SVHigh4/1120)/(Change^2)
  return(Answer)
}
#
# Second derivative wrt BC level (numerical) (Identical to FC level)
#
NSDwrtBCL = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original <- dfSWAP$GRParm1
  Change <- Increment*Original
  if(Change == 0)Change <- 0.01
  High4 <- Original + 4.0*Change
  dfSWAP$GRParm1 <- High4
  SVHigh4 <- SwapValuettd(dfSWAP, dfHDInputs)
  High3 <- Original + 3.0*Change
  dfSWAP$GRParm1 <- High3
  SVHigh3 <- SwapValuettd(dfSWAP, dfHDInputs)
  High2 <- Original + 2.0*Change
  dfSWAP$GRParm1 <- High2
  SVHigh2 <- SwapValuettd(dfSWAP, dfHDInputs)
  High1 <- Original + 1.0*Change
  dfSWAP$GRParm1 <- High1
  SVHigh1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$GRParm1 <- Original
  SVOriginal <- SwapValuettd(dfSWAP, dfHDInputs)
  Low4 <- Original - 4.0*Change
  dfSWAP$GRParm1 <- Low4
  SVLow4 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low3 <- Original - 3.0*Change
  dfSWAP$GRParm1 <- Low3
  SVLow3 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low2 <- Original - 2.0*Change
  dfSWAP$GRParm1 <- Low2
  SVLow2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low1 <- Original - 1.0*Change
  dfSWAP$GRParm1 <- Low1
  SVLow1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$GRParm1 <- Original
  Answer <- 2*(-SVLow4/1120 + 4*SVLow3/315 - SVLow2/10 + 4*SVLow1/5 - 
    205*SVOriginal/144 +
    4*SVHigh1/5 - SVHigh2/10 + 4*SVHigh3/315 -SVHigh4/1120)/(Change^2)
  return(Answer)  
}
#
# Second derivative wrt BC slope (numerical)
#
NSDwrtBCS = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original <- dfSWAP$GRParm2
  Change <- Increment*Original
  if(Change == 0)Change <- 0.01
  High4 <- Original + 4.0*Change
  dfSWAP$GRParm2 <- High4
  SVHigh4 <- SwapValuettd(dfSWAP, dfHDInputs)
  High3 <- Original + 3.0*Change
  dfSWAP$GRParm2 <- High3
  SVHigh3 <- SwapValuettd(dfSWAP, dfHDInputs)
  High2 <- Original + 2.0*Change
  dfSWAP$GRParm2 <- High2
  SVHigh2 <- SwapValuettd(dfSWAP, dfHDInputs)
  High1 <- Original + 1.0*Change
  dfSWAP$GRParm2 <- High1
  SVHigh1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$GRParm2 <- Original
  SVOriginal <- SwapValuettd(dfSWAP, dfHDInputs)
  Low4 <- Original - 4.0*Change
  dfSWAP$GRParm2 <- Low4
  SVLow4 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low3 <- Original - 3.0*Change
  dfSWAP$GRParm2 <- Low3
  SVLow3 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low2 <- Original - 2.0*Change
  dfSWAP$GRParm2 <- Low2
  SVLow2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low1 <- Original - 1.0*Change
  dfSWAP$GRParm2 <- Low1
  SVLow1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$GRParm2 <- Original
  Answer <- 2*(-SVLow4/1120 + 4*SVLow3/315 - SVLow2/10 + 4*SVLow1/5 - 
    205*SVOriginal/144 +
    4*SVHigh1/5 - SVHigh2/10 + 4*SVHigh3/315 -SVHigh4/1120)/(Change^2)
  return(Answer)
}
#
# Second derivative wrt BC curvature 1 (numerical)
#
NSDwrtBCC1 = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original <- dfSWAP$GRParm3
  Change <- Increment*Original
  if(Change == 0)Change <- 0.01
  High4 <- Original + 4.0*Change
  dfSWAP$GRParm3 <- High4
  SVHigh4 <- SwapValuettd(dfSWAP, dfHDInputs)
  High3 <- Original + 3.0*Change
  dfSWAP$GRParm3 <- High3
  SVHigh3 <- SwapValuettd(dfSWAP, dfHDInputs)
  High2 <- Original + 2.0*Change
  dfSWAP$GRParm3 <- High2
  SVHigh2 <- SwapValuettd(dfSWAP, dfHDInputs)
  High1 <- Original + 1.0*Change
  dfSWAP$GRParm3 <- High1
  SVHigh1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$GRParm3 <- Original
  SVOriginal <- SwapValuettd(dfSWAP, dfHDInputs)
  Low4 <- Original - 4.0*Change
  dfSWAP$GRParm3 <- Low4
  SVLow4 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low3 <- Original - 3.0*Change
  dfSWAP$GRParm3 <- Low3
  SVLow3 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low2 <- Original - 2.0*Change
  dfSWAP$GRParm3 <- Low2
  SVLow2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Low1 <- Original - 1.0*Change
  dfSWAP$GRParm3 <- Low1
  SVLow1 <- SwapValuettd(dfSWAP, dfHDInputs)
  dfSWAP$GRParm3 <- Original
  Answer <- 2*(-SVLow4/1120 + 4*SVLow3/315 - SVLow2/10 + 4*SVLow1/5 - 
    205*SVOriginal/144 +
    4*SVHigh1/5 - SVHigh2/10 + 4*SVHigh3/315 -SVHigh4/1120)/(Change^2)
  return(Answer)
}
#
# Second derivative wrt FC level
#
SDwrtFCL = function(dfSWAP, dfHDInputs){
  with(dfSWAP, {
    EM <- dfHDInputs$HM
    ED <- dfHDInputs$HD
    EY <- dfHDInputs$HY
# Set main parameters for LSC forward rates
    Maturity = 0.00001 # Placeholder, not used
    NumberOfFactors = NLSCFR
    Intercept = FRParm1
    Slope = FRParm2
    Curvature1 = FRParm3
    Curvature2 = FRParm4
    Curvature3 = FRParm5
    Curvature4 = FRParm6
    Tau1 = FRScalar1
    Tau2 = FRScalar2
    Tau3 = FRScalar3
    Tau4 = FRScalar4
    Tau5 = FRScalar5
    LSCFC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCFC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")    
# Set main parameters for LSC generic rates
    NumberOfFactors = NLSCGR
    Intercept = GRParm1
    Slope = GRParm2
    Curvature1 = GRParm3
    Curvature2 = GRParm4
    Curvature3 = GRParm5
    Curvature4 = GRParm6
    Tau1 = GRScalar1
    Tau2 = GRScalar2
    Tau3 = GRScalar3
    Tau4 = GRScalar4
    Tau5 = GRScalar5
    LSCGC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCGC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FltConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FltPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FltData <- PaymentsRemaining(NData)
    nFlt <- FltData$Counter
    CFDatesFlt <- as.date(FltData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFlt <- sort(CFDatesFlt[!is.na(CFDatesFlt)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFlt <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      if(i>1){
        LSCFC$Maturity = (CFDatesFlt[i] - EvaluationDateFlt)/365 
      } else {
        LSCFC$Maturity = 0.00001
      }
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      TDF <- (LSCFC$Maturity^2)*DF[i] + TDF
    }
    TermFlt1 <- NAmt*TDF
# Reset vectors for TermFlt2    
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      TDF <- (LSCGC$Maturity^2)*DF[i] + TDF
    }
    TermFlt2 <- NAmt*TDF
# TermFix    
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FixConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FixPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FixData <- PaymentsRemaining(NData)
    nFix <- FixData$Counter
    CFDatesFix <- as.date(FixData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFix <- sort(CFDatesFix[!is.na(CFDatesFix)])
    # Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFix <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFix))
    FR[] <- 0
    BR <- FR
    DF <- FR
    APFix <- FR
    NADFix <- APFix
    TDF <- 0.0
    for(i in 1:nFix){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFix[i+1] - EvaluationDateFix)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      if(FixNAD < 1) NADFix[i] = CFDatesFix[i+1] - CFDatesFix[i]
      else {
        if(FixPF == 1) NADFix[i] = 360
        else if (FixPF == 2) NADFix[i] = 180
        else if (FixPF == 4) NADFix[i] = 90
        else NADFix[i] = 30
      }
      APFix[i] = NADFix[i]/FixNTD
      TDF <- (LSCGC$Maturity^2)*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })
}
#
# Second derivative wrt FC slope
#
SDwrtFCS = function(dfSWAP, dfHDInputs){
  with(dfSWAP, {
    EM <- dfHDInputs$HM
    ED <- dfHDInputs$HD
    EY <- dfHDInputs$HY
# Set main parameters for LSC forward rates
    Maturity = 0.00001 # Placeholder, not used
    NumberOfFactors = NLSCFR
    Intercept = FRParm1
    Slope = FRParm2
    Curvature1 = FRParm3
    Curvature2 = FRParm4
    Curvature3 = FRParm5
    Curvature4 = FRParm6
    Tau1 = FRScalar1
    Tau2 = FRScalar2
    Tau3 = FRScalar3
    Tau4 = FRScalar4
    Tau5 = FRScalar5
    LSCFC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCFC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")    
# Set main parameters for LSC generic rates
    NumberOfFactors = NLSCGR
    Intercept = GRParm1
    Slope = GRParm2
    Curvature1 = GRParm3
    Curvature2 = GRParm4
    Curvature3 = GRParm5
    Curvature4 = GRParm6
    Tau1 = GRScalar1
    Tau2 = GRScalar2
    Tau3 = GRScalar3
    Tau4 = GRScalar4
    Tau5 = GRScalar5
    LSCGC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCGC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FltConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FltPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FltData <- PaymentsRemaining(NData)
    nFlt <- FltData$Counter
    CFDatesFlt <- as.date(FltData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFlt <- sort(CFDatesFlt[!is.na(CFDatesFlt)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFlt <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      if(i>1){
        LSCFC$Maturity = (CFDatesFlt[i] - EvaluationDateFlt)/365 
      } else {
        LSCFC$Maturity = 0.00001
      }
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      x <- ((1.0 - exp(-LSCFC$Maturity/LSCFC$Tau1))/(LSCFC$Maturity/LSCFC$Tau1))
      TDF <- ((x*LSCFC$Maturity)^2)*DF[i] + TDF
    }
    TermFlt1 <- NAmt*TDF
# Reset vectors for TermFlt2    
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      x <- ((1.0 - exp(-LSCFC$Maturity/LSCFC$Tau1))/(LSCFC$Maturity/LSCFC$Tau1))
      TDF <- ((x*LSCFC$Maturity)^2)*DF[i] + TDF
    }
    TermFlt2 <- NAmt*TDF
# TermFix    
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FixConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FixPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FixData <- PaymentsRemaining(NData)
    nFix <- FixData$Counter
    CFDatesFix <- as.date(FixData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFix <- sort(CFDatesFix[!is.na(CFDatesFix)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFix <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFix))
    FR[] <- 0
    BR <- FR
    DF <- FR
    APFix <- FR
    NADFix <- APFix
    TDF <- 0.0
    for(i in 1:nFix){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFix[i+1] - EvaluationDateFix)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      if(FixNAD < 1) NADFix[i] = CFDatesFix[i+1] - CFDatesFix[i]
      else {
        if(FixPF == 1) NADFix[i] = 360
        else if (FixPF == 2) NADFix[i] = 180
        else if (FixPF == 4) NADFix[i] = 90
        else NADFix[i] = 30
      }
      APFix[i] = NADFix[i]/FixNTD
      x <- ((1.0 - exp(-LSCFC$Maturity/LSCFC$Tau1))/(LSCFC$Maturity/LSCFC$Tau1))
      TDF <- ((x*LSCFC$Maturity)^2)*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })
}
#
# Second derivative wrt FC curvature 1
#
SDwrtFCC1 = function(dfSWAP, dfHDInputs){
  with(dfSWAP, {
    EM <- dfHDInputs$HM
    ED <- dfHDInputs$HD
    EY <- dfHDInputs$HY
    # Set main parameters for LSC forward rates
    Maturity = 0.00001 # Placeholder, not used
    NumberOfFactors = NLSCFR
    Intercept = FRParm1
    Slope = FRParm2
    Curvature1 = FRParm3
    Curvature2 = FRParm4
    Curvature3 = FRParm5
    Curvature4 = FRParm6
    Tau1 = FRScalar1
    Tau2 = FRScalar2
    Tau3 = FRScalar3
    Tau4 = FRScalar4
    Tau5 = FRScalar5
    LSCFC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCFC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")    
    # Set main parameters for LSC generic rates
    NumberOfFactors = NLSCGR
    Intercept = GRParm1
    Slope = GRParm2
    Curvature1 = GRParm3
    Curvature2 = GRParm4
    Curvature3 = GRParm5
    Curvature4 = GRParm6
    Tau1 = GRScalar1
    Tau2 = GRScalar2
    Tau3 = GRScalar3
    Tau4 = GRScalar4
    Tau5 = GRScalar5
    LSCGC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCGC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FltConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FltPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FltData <- PaymentsRemaining(NData)
    nFlt <- FltData$Counter
    CFDatesFlt <- as.date(FltData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFlt <- sort(CFDatesFlt[!is.na(CFDatesFlt)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFlt <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      if(i>1){
        LSCFC$Maturity = (CFDatesFlt[i] - EvaluationDateFlt)/365 
      } else {
        LSCFC$Maturity = 0.00001
      }
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      x <- ((1.0 - exp(-LSCFC$Maturity/LSCFC$Tau2))/(LSCFC$Maturity/LSCFC$Tau2) -
        exp(-LSCFC$Maturity/LSCFC$Tau2))
      TDF <- ((x*LSCFC$Maturity)^2)*DF[i] + TDF
    }
    TermFlt1 <- NAmt*TDF
    # Reset vectors for TermFlt2    
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      x <- ((1.0 - exp(-LSCFC$Maturity/LSCFC$Tau2))/(LSCFC$Maturity/LSCFC$Tau2) -
        exp(-LSCFC$Maturity/LSCFC$Tau2))
      TDF <- ((x*LSCFC$Maturity)^2)*DF[i] + TDF
    }
    TermFlt2 <- NAmt*TDF
# TermFix    
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FixConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FixPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FixData <- PaymentsRemaining(NData)
    nFix <- FixData$Counter
    CFDatesFix <- as.date(FixData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFix <- sort(CFDatesFix[!is.na(CFDatesFix)])
    # Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFix <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFix))
    FR[] <- 0
    BR <- FR
    DF <- FR
    APFix <- FR
    NADFix <- APFix
    TDF <- 0.0
    for(i in 1:nFix){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFix[i+1] - EvaluationDateFix)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      if(FixNAD < 1) NADFix[i] = CFDatesFix[i+1] - CFDatesFix[i]
      else {
        if(FixPF == 1) NADFix[i] = 360
        else if (FixPF == 2) NADFix[i] = 180
        else if (FixPF == 4) NADFix[i] = 90
        else NADFix[i] = 30
      }
      APFix[i] = NADFix[i]/FixNTD
      x <- ((1.0 - exp(-LSCFC$Maturity/LSCFC$Tau2))/(LSCFC$Maturity/LSCFC$Tau2) -
        exp(-LSCFC$Maturity/LSCFC$Tau2))
      TDF <- ((x*LSCFC$Maturity)^2)*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })
}
#
# Second derivative wrt BC level (identical to FC level)
#
SDwrtBCL = function(dfSWAP, dfHDInputs){
  with(dfSWAP, {
    EM <- dfHDInputs$HM
    ED <- dfHDInputs$HD
    EY <- dfHDInputs$HY
# Set main parameters for LSC forward rates
    Maturity = 0.00001 # Placeholder, not used
    NumberOfFactors = NLSCFR
    Intercept = FRParm1
    Slope = FRParm2
    Curvature1 = FRParm3
    Curvature2 = FRParm4
    Curvature3 = FRParm5
    Curvature4 = FRParm6
    Tau1 = FRScalar1
    Tau2 = FRScalar2
    Tau3 = FRScalar3
    Tau4 = FRScalar4
    Tau5 = FRScalar5
    LSCFC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCFC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")    
# Set main parameters for LSC generic rates
    NumberOfFactors = NLSCGR
    Intercept = GRParm1
    Slope = GRParm2
    Curvature1 = GRParm3
    Curvature2 = GRParm4
    Curvature3 = GRParm5
    Curvature4 = GRParm6
    Tau1 = GRScalar1
    Tau2 = GRScalar2
    Tau3 = GRScalar3
    Tau4 = GRScalar4
    Tau5 = GRScalar5
    LSCGC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCGC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FltConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FltPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FltData <- PaymentsRemaining(NData)
    nFlt <- FltData$Counter
    CFDatesFlt <- as.date(FltData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFlt <- sort(CFDatesFlt[!is.na(CFDatesFlt)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFlt <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      if(i>1){
        LSCFC$Maturity = (CFDatesFlt[i] - EvaluationDateFlt)/365 
      } else {
        LSCFC$Maturity = 0.00001
      }
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      TDF <- (LSCGC$Maturity^2)*DF[i] + TDF
    }
    TermFlt1 <- NAmt*TDF
# Reset vectors for TermFlt2    
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      TDF <- (LSCGC$Maturity^2)*DF[i] + TDF
    }
    TermFlt2 <- NAmt*TDF
# TermFix    
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FixConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FixPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FixData <- PaymentsRemaining(NData)
    nFix <- FixData$Counter
    CFDatesFix <- as.date(FixData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFix <- sort(CFDatesFix[!is.na(CFDatesFix)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFix <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFix))
    FR[] <- 0
    BR <- FR
    DF <- FR
    APFix <- FR
    NADFix <- APFix
    TDF <- 0.0
    for(i in 1:nFix){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFix[i+1] - EvaluationDateFix)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      if(FixNAD < 1) NADFix[i] = CFDatesFix[i+1] - CFDatesFix[i]
      else {
        if(FixPF == 1) NADFix[i] = 360
        else if (FixPF == 2) NADFix[i] = 180
        else if (FixPF == 4) NADFix[i] = 90
        else NADFix[i] = 30
      }
      APFix[i] = NADFix[i]/FixNTD
      TDF <- (LSCGC$Maturity^2)*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })  
}
#
# Second derivative wrt BC slope
#
SDwrtBCS = function(dfSWAP, dfHDInputs){
  with(dfSWAP, {
    EM <- dfHDInputs$HM
    ED <- dfHDInputs$HD
    EY <- dfHDInputs$HY
    # Set main parameters for LSC forward rates
    Maturity = 0.00001 # Placeholder, not used
    NumberOfFactors = NLSCFR
    Intercept = FRParm1
    Slope = FRParm2
    Curvature1 = FRParm3
    Curvature2 = FRParm4
    Curvature3 = FRParm5
    Curvature4 = FRParm6
    Tau1 = FRScalar1
    Tau2 = FRScalar2
    Tau3 = FRScalar3
    Tau4 = FRScalar4
    Tau5 = FRScalar5
    LSCFC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCFC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")    
# Set main parameters for LSC generic rates
    NumberOfFactors = NLSCGR
    Intercept = GRParm1
    Slope = GRParm2
    Curvature1 = GRParm3
    Curvature2 = GRParm4
    Curvature3 = GRParm5
    Curvature4 = GRParm6
    Tau1 = GRScalar1
    Tau2 = GRScalar2
    Tau3 = GRScalar3
    Tau4 = GRScalar4
    Tau5 = GRScalar5
    LSCGC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCGC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FltConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FltPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FltData <- PaymentsRemaining(NData)
    nFlt <- FltData$Counter
    CFDatesFlt <- as.date(FltData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFlt <- sort(CFDatesFlt[!is.na(CFDatesFlt)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFlt <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      if(i>1){
        LSCFC$Maturity = (CFDatesFlt[i] - EvaluationDateFlt)/365 
      } else {
        LSCFC$Maturity = 0.00001
      }
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      x <- ((1.0 - exp(-LSCGC$Maturity/LSCGC$Tau1))/(LSCGC$Maturity/LSCGC$Tau1))
      TDF <- ((x*LSCGC$Maturity)^2)*DF[i] + TDF
    }
    TermFlt1 <- NAmt*TDF
# Reset vectors for TermFlt2    
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      x <- ((1.0 - exp(-LSCGC$Maturity/LSCGC$Tau1))/(LSCGC$Maturity/LSCGC$Tau1))
      TDF <- ((x*LSCGC$Maturity)^2)*DF[i] + TDF
    }
    TermFlt2 <- NAmt*TDF
# TermFix    
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FixConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FixPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FixData <- PaymentsRemaining(NData)
    nFix <- FixData$Counter
    CFDatesFix <- as.date(FixData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFix <- sort(CFDatesFix[!is.na(CFDatesFix)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFix <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFix))
    FR[] <- 0
    BR <- FR
    DF <- FR
    APFix <- FR
    NADFix <- APFix
    TDF <- 0.0
    for(i in 1:nFix){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFix[i+1] - EvaluationDateFix)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      if(FixNAD < 1) NADFix[i] = CFDatesFix[i+1] - CFDatesFix[i]
      else {
        if(FixPF == 1) NADFix[i] = 360
        else if (FixPF == 2) NADFix[i] = 180
        else if (FixPF == 4) NADFix[i] = 90
        else NADFix[i] = 30
      }
      APFix[i] = NADFix[i]/FixNTD
      x <- ((1.0 - exp(-LSCGC$Maturity/LSCGC$Tau1))/(LSCGC$Maturity/LSCGC$Tau1))
      TDF <- ((x*LSCGC$Maturity)^2)*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })
}
#
# Second derivative wrt BC curvature 1
#
SDwrtBCC1 = function(dfSWAP, dfHDInputs){
  with(dfSWAP, {
    EM <- dfHDInputs$HM
    ED <- dfHDInputs$HD
    EY <- dfHDInputs$HY
    # Set main parameters for LSC forward rates
    Maturity = 0.00001 # Placeholder, not used
    NumberOfFactors = NLSCFR
    Intercept = FRParm1
    Slope = FRParm2
    Curvature1 = FRParm3
    Curvature2 = FRParm4
    Curvature3 = FRParm5
    Curvature4 = FRParm6
    Tau1 = FRScalar1
    Tau2 = FRScalar2
    Tau3 = FRScalar3
    Tau4 = FRScalar4
    Tau5 = FRScalar5
    LSCFC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCFC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")    
# Set main parameters for LSC generic rates
    NumberOfFactors = NLSCGR
    Intercept = GRParm1
    Slope = GRParm2
    Curvature1 = GRParm3
    Curvature2 = GRParm4
    Curvature3 = GRParm5
    Curvature4 = GRParm6
    Tau1 = GRScalar1
    Tau2 = GRScalar2
    Tau3 = GRScalar3
    Tau4 = GRScalar4
    Tau5 = GRScalar5
    LSCGC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, 
      Curvature2, Curvature3, Curvature4, Tau1, Tau2, Tau3, Tau4, Tau5)
    names(LSCGC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", 
      "Curvature1", "Curvature2", "Curvature3", "Curvature4", 
      "Tau1", "Tau2", "Tau3", "Tau4", "Tau5")
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FltConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FltPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FltData <- PaymentsRemaining(NData)
    nFlt <- FltData$Counter
    CFDatesFlt <- as.date(FltData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFlt <- sort(CFDatesFlt[!is.na(CFDatesFlt)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFlt <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      if(i>1){
        LSCFC$Maturity = (CFDatesFlt[i] - EvaluationDateFlt)/365 
      } else {
        LSCFC$Maturity = 0.00001
      }
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      x <- ((1.0 - exp(-LSCGC$Maturity/LSCGC$Tau2))/(LSCGC$Maturity/LSCGC$Tau2) -
        exp(-LSCGC$Maturity/LSCGC$Tau2))
      TDF <- ((x*LSCGC$Maturity)^2)*DF[i] + TDF
    }
    TermFlt1 <- NAmt*TDF
# Reset vectors for TermFlt2    
    FR <- as.numeric(c(1:nFlt))
    FR[] <- 0
    BR <- FR
    DF <- FR
    TDF <- 0.0
    for(i in 1:nFlt){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFlt[i+1] - EvaluationDateFlt)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      x <- ((1.0 - exp(-LSCGC$Maturity/LSCGC$Tau2))/(LSCGC$Maturity/LSCGC$Tau2) -
        exp(-LSCGC$Maturity/LSCGC$Tau2))
      TDF <- ((x*LSCGC$Maturity)^2)*DF[i] + TDF
    }
    TermFlt2 <- NAmt*TDF
# TermFix    
# Either Modified Business Following (MBF), Modified Business Preceding (MBP)
    Convention <- FixConv 
    NData <- list(EM, ED, EY, MM, MD, MY, Convention, FixPF)
    names(NData) <- c("SettlementDateMonth", "SettlementDateDay", 
      "SettlementDateYear", "MaturityDateMonth", "MaturityDateDay", 
      "MaturityDateYear", "Convention", "Frequency")
    FixData <- PaymentsRemaining(NData)
    nFix <- FixData$Counter
    CFDatesFix <- as.date(FixData$PayDates) # CFDatesFlt[1] is evaluation date
    CFDatesFix <- sort(CFDatesFix[!is.na(CFDatesFix)])
# Stub analysis
    JEvaluationDate <- AdjustDate(EM, ED, EY, Convention)
    EvaluationDateFix <- as.date(as.integer(JEvaluationDate))
    FR <- as.numeric(c(1:nFix))
    FR[] <- 0
    BR <- FR
    DF <- FR
    APFix <- FR
    NADFix <- APFix
    TDF <- 0.0
    for(i in 1:nFix){  # CFDatesFlt[1] is evaluation date
      LSCFC$Maturity = LSCGC$Maturity = (CFDatesFix[i+1] - EvaluationDateFix)/365
      BR[i] = LSCRate(LSCGC)/100.0
      FR[i] = LSCRate(LSCFC)/100.0
      DF[i] = exp(-(BR[i]*LSCGC$Maturity + FR[i]*LSCFC$Maturity))
      if(FixNAD < 1) NADFix[i] = CFDatesFix[i+1] - CFDatesFix[i]
      else {
        if(FixPF == 1) NADFix[i] = 360
        else if (FixPF == 2) NADFix[i] = 180
        else if (FixPF == 4) NADFix[i] = 90
        else NADFix[i] = 30
      }
      APFix[i] = NADFix[i]/FixNTD
      x <- ((1.0 - exp(-LSCGC$Maturity/LSCGC$Tau2))/(LSCGC$Maturity/LSCGC$Tau2) -
              exp(-LSCGC$Maturity/LSCGC$Tau2))
      TDF <- ((x*LSCGC$Maturity)^2)*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })
}

