# SRM SWAP Derivatives Functions.R
#
# First derivative wrt FC level (numerical)
#
NFDwrtFCL = function(dfSWAP, dfHDInputs){
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
  Answer <- (SVLow4/280 - 4*SVLow3/105 + SVLow2/5 - 4*SVLow1/5
    + 4*SVHigh1/5 - SVHigh2/5 + 4*SVHigh3/105 -SVHigh4/280)/Change
  return(Answer)
}
#
# First derivative wrt FC slope (numerical)
#
NFDwrtFCS = function(dfSWAP, dfHDInputs){
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
  Answer <- (SVLow4/280 - 4*SVLow3/105 + SVLow2/5 - 4*SVLow1/5
    + 4*SVHigh1/5 - SVHigh2/5 + 4*SVHigh3/105 -SVHigh4/280)/Change
  return(Answer)
}
#
# First derivative wrt FC curvature 1 (numerical)
#
NFDwrtFCC1 = function(dfSWAP, dfHDInputs){
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
  Answer <- (SVLow4/280 - 4*SVLow3/105 + SVLow2/5 - 4*SVLow1/5
    + 4*SVHigh1/5 - SVHigh2/5 + 4*SVHigh3/105 -SVHigh4/280)/Change
  return(Answer)
}
#
# First derivative wrt BC level (numerical) (Identical to FC level)
#
NFDwrtBCL = function(dfSWAP, dfHDInputs){
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
  Answer <- (SVLow4/280 - 4*SVLow3/105 + SVLow2/5 - 4*SVLow1/5
    + 4*SVHigh1/5 - SVHigh2/5 + 4*SVHigh3/105 -SVHigh4/280)/Change
  return(Answer)
}
#
# First derivative wrt BC slope (numerical)
#
NFDwrtBCS = function(dfSWAP, dfHDInputs){
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
  Answer <- (SVLow4/280 - 4*SVLow3/105 + SVLow2/5 - 4*SVLow1/5
    + 4*SVHigh1/5 - SVHigh2/5 + 4*SVHigh3/105 -SVHigh4/280)/Change
  return(Answer)
}
#
# First derivative wrt BC curvature 1 (numerical)
#
NFDwrtBCC1 = function(dfSWAP, dfHDInputs){
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
  Answer <- (SVLow4/280 - 4*SVLow3/105 + SVLow2/5 - 4*SVLow1/5
    + 4*SVHigh1/5 - SVHigh2/5 + 4*SVHigh3/105 -SVHigh4/280)/Change
  return(Answer)
}
# NFDwrtFCL = function(dfSWAP, dfHDInputs){
#   Increment <- 0.01
#   Original <- dfSWAP$FRParm1
#   Change <- Increment*Original
#   High <- Original + Change
#   dfSWAP$FRParm1 <- High
#   SVHigh <- SwapValuettd(dfSWAP, dfHDInputs)
#   Low <- Original - Change
#   dfSWAP$FRParm1 <- Low
#   SVLow <- SwapValuettd(dfSWAP, dfHDInputs)
#   dfSWAP$FRParm1 <- Original
#   Answer <- (SVHigh - SVLow)/(High - Low)
#   return(Answer)
# }

#
# First derivative wrt FC level
#
FDwrtFCL = function(dfSWAP, dfHDInputs){
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
      TDF <- LSCFC$Maturity*DF[i] + TDF
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
      TDF <- LSCGC$Maturity*DF[i] + TDF
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
      TDF <- LSCGC$Maturity*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    FDSV <- dfSWAP$SwapType*(TermFlt1 - TermFlt2 - (dfSWAP$FixedRate/100)*TermFix) 
    return( FDSV/100.0 )
  })
}
#
# First derivative wrt FC slope
#
FDwrtFCS = function(dfSWAP, dfHDInputs){
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
      TDF <- x*LSCFC$Maturity*DF[i] + TDF
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
      TDF <- x*LSCFC$Maturity*DF[i] + TDF
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
      TDF <- x*LSCFC$Maturity*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    FDSV <- dfSWAP$SwapType*(TermFlt1 - TermFlt2 - (dfSWAP$FixedRate/100)*TermFix) 
    return( FDSV/100.0 )
  })
}
#
# First derivative wrt FC curvature
#
FDwrtFCC1 = function(dfSWAP, dfHDInputs){
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
      TDF <- x*LSCFC$Maturity*DF[i] + TDF
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
      TDF <- x*LSCFC$Maturity*DF[i] + TDF
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
      TDF <- x*LSCFC$Maturity*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    FDSV <- dfSWAP$SwapType*(TermFlt1 - TermFlt2 - (dfSWAP$FixedRate/100)*TermFix) 
    return( FDSV/100.0 )
  })
}
#
# First derivative wrt BC level (identical to FC level)
#
FDwrtBCL = function(dfSWAP, dfHDInputs){
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
      TDF <- LSCGC$Maturity*DF[i] + TDF
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
      TDF <- LSCGC$Maturity*DF[i] + TDF
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
      TDF <- LSCGC$Maturity*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    FDSV <- dfSWAP$SwapType*(TermFlt1 - TermFlt2 - (dfSWAP$FixedRate/100)*TermFix) 
    return( FDSV/100.0 )
  })
}
#
# First derivative wrt BC slope
#
FDwrtBCS = function(dfSWAP, dfHDInputs){
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
      TDF <- x*LSCGC$Maturity*DF[i] + TDF
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
      TDF <- x*LSCGC$Maturity*DF[i] + TDF
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
      TDF <- x*LSCGC$Maturity*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    FDSV <- dfSWAP$SwapType*(TermFlt1 - TermFlt2 - (dfSWAP$FixedRate/100)*TermFix) 
    return( FDSV/100.0 )
  })
}
#
# First derivative wrt BC curvature 1
#
FDwrtBCC1 = function(dfSWAP, dfHDInputs){
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
      TDF <- x*LSCGC$Maturity*DF[i] + TDF
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
      TDF <- x*LSCGC$Maturity*DF[i] + TDF
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
      TDF <- x*LSCGC$Maturity*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    FDSV <- dfSWAP$SwapType*(TermFlt1 - TermFlt2 - (dfSWAP$FixedRate/100)*TermFix) 
    return( FDSV/100.0 )
  })
}



