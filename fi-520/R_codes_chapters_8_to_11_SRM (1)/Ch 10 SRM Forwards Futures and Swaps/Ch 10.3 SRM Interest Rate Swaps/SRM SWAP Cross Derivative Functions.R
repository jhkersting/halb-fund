# SRM SWAP Cross Derivative Functions.R
#
# Second derivative wrt FC level and FC slope (numerical)
#
NSDwrtFCLS = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original1 <- dfSWAP$FRParm1
  Original2 <- dfSWAP$FRParm2
  Change1 <- Increment*Original1
  Change2 <- Increment*Original2
  if(Change1 == 0)Change1 <- 0.01
  if(Change2 == 0)Change2 <- 0.01
  V1 <- Original1 + Change1
  V2 <- Original2 + Change2
  dfSWAP$FRParm1 <- V1
  dfSWAP$FRParm2 <- V2
  SVP1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 + Change1
  V2 <- Original2 - Change2
  dfSWAP$FRParm1 <- V1
  dfSWAP$FRParm2 <- V2
  SVP1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 + Change2
  dfSWAP$FRParm1 <- V1
  dfSWAP$FRParm2 <- V2
  SVM1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 - Change2
  dfSWAP$FRParm1 <- V1
  dfSWAP$FRParm2 <- V2
  SVM1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Answer <- (SVP1P2 - SVP1M2 - SVM1P2 + SVM1M2)/(4*Change1*Change2)
  return(Answer)
}
#
# Second derivative wrt FC slope and FC curvature 1 (numerical)
#
NSDwrtFCSC1 = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original1 <- dfSWAP$FRParm2
  Original2 <- dfSWAP$FRParm3
  Change1 <- Increment*Original1
  Change2 <- Increment*Original2
  if(Change1 == 0)Change1 <- 0.01
  if(Change2 == 0)Change2 <- 0.01
  V1 <- Original1 + Change1
  V2 <- Original2 + Change2
  dfSWAP$FRParm2 <- V1
  dfSWAP$FRParm3 <- V2
  SVP1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 + Change1
  V2 <- Original2 - Change2
  dfSWAP$FRParm2 <- V1
  dfSWAP$FRParm3 <- V2
  SVP1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 + Change2
  dfSWAP$FRParm2 <- V1
  dfSWAP$FRParm3 <- V2
  SVM1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 - Change2
  dfSWAP$FRParm2 <- V1
  dfSWAP$FRParm3 <- V2
  SVM1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Answer <- (SVP1P2 - SVP1M2 - SVM1P2 + SVM1M2)/(4*Change1*Change2)
  return(Answer)
}
#
# Second derivative wrt FC level and FC curvature 1 (numerical)
#
NSDwrtFCLC1 = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original1 <- dfSWAP$FRParm1
  Original2 <- dfSWAP$FRParm3
  Change1 <- Increment*Original1
  Change2 <- Increment*Original2
  if(Change1 == 0)Change1 <- 0.01
  if(Change2 == 0)Change2 <- 0.01
  V1 <- Original1 + Change1
  V2 <- Original2 + Change2
  dfSWAP$FRParm1 <- V1
  dfSWAP$FRParm3 <- V2
  SVP1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 + Change1
  V2 <- Original2 - Change2
  dfSWAP$FRParm1 <- V1
  dfSWAP$FRParm3 <- V2
  SVP1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 + Change2
  dfSWAP$FRParm1 <- V1
  dfSWAP$FRParm3 <- V2
  SVM1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 - Change2
  dfSWAP$FRParm1 <- V1
  dfSWAP$FRParm3 <- V2
  SVM1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Answer <- (SVP1P2 - SVP1M2 - SVM1P2 + SVM1M2)/(4*Change1*Change2)
  return(Answer)
}
#
# Second derivative wrt BC level and BC slope (numerical) (NOT Identical to FC level)
#
NSDwrtBCLS = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original1 <- dfSWAP$GRParm1
  Original2 <- dfSWAP$GRParm2
  Change1 <- Increment*Original1
  Change2 <- Increment*Original2
  if(Change1 == 0)Change1 <- 0.01
  if(Change2 == 0)Change2 <- 0.01
  V1 <- Original1 + Change1
  V2 <- Original2 + Change2
  dfSWAP$GRParm1 <- V1
  dfSWAP$GRParm2 <- V2
  SVP1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 + Change1
  V2 <- Original2 - Change2
  dfSWAP$GRParm1 <- V1
  dfSWAP$GRParm2 <- V2
  SVP1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 + Change2
  dfSWAP$GRParm1 <- V1
  dfSWAP$GRParm2 <- V2
  SVM1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 - Change2
  dfSWAP$GRParm1 <- V1
  dfSWAP$GRParm2 <- V2
  SVM1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Answer <- (SVP1P2 - SVP1M2 - SVM1P2 + SVM1M2)/(4*Change1*Change2)
  return(Answer)
}
#
# Second derivative wrt BC slope and BC curvature 1 (numerical)
#
NSDwrtBCSC1 = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original1 <- dfSWAP$GRParm2
  Original2 <- dfSWAP$GRParm3
  Change1 <- Increment*Original1
  Change2 <- Increment*Original2
  if(Change1 == 0)Change1 <- 0.01
  if(Change2 == 0)Change2 <- 0.01
  V1 <- Original1 + Change1
  V2 <- Original2 + Change2
  dfSWAP$GRParm2 <- V1
  dfSWAP$GRParm3 <- V2
  SVP1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 + Change1
  V2 <- Original2 - Change2
  dfSWAP$GRParm2 <- V1
  dfSWAP$GRParm3 <- V2
  SVP1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 + Change2
  dfSWAP$GRParm2 <- V1
  dfSWAP$GRParm3 <- V2
  SVM1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 - Change2
  dfSWAP$GRParm2 <- V1
  dfSWAP$GRParm3 <- V2
  SVM1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Answer <- (SVP1P2 - SVP1M2 - SVM1P2 + SVM1M2)/(4*Change1*Change2)
  return(Answer)
}
#
# Second derivative wrt BC level and BC curvature 1 (numerical)
#
NSDwrtBCLC1 = function(dfSWAP, dfHDInputs){
  Increment <- 0.01
  Original1 <- dfSWAP$GRParm1
  Original2 <- dfSWAP$GRParm3
  Change1 <- Increment*Original1
  Change2 <- Increment*Original2
  if(Change1 == 0)Change1 <- 0.01
  if(Change2 == 0)Change2 <- 0.01
  V1 <- Original1 + Change1
  V2 <- Original2 + Change2
  dfSWAP$GRParm1 <- V1
  dfSWAP$GRParm3 <- V2
  SVP1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 + Change1
  V2 <- Original2 - Change2
  dfSWAP$GRParm1 <- V1
  dfSWAP$GRParm3 <- V2
  SVP1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 + Change2
  dfSWAP$GRParm1 <- V1
  dfSWAP$GRParm3 <- V2
  SVM1P2 <- SwapValuettd(dfSWAP, dfHDInputs)
  V1 <- Original1 - Change1
  V2 <- Original2 - Change2
  dfSWAP$GRParm1 <- V1
  dfSWAP$GRParm3 <- V2
  SVM1M2 <- SwapValuettd(dfSWAP, dfHDInputs)
  Answer <- (SVP1P2 - SVP1M2 - SVM1P2 + SVM1M2)/(4*Change1*Change2)
  return(Answer)
}
#
# Second derivative wrt FC level and FC slope
#
SDwrtFCLS = function(dfSWAP, dfHDInputs){
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
      x <- (1.0 - exp(-LSCFC$Maturity/LSCFC$Tau1))/(LSCFC$Maturity/LSCFC$Tau1)
      TDF <- (LSCFC$Maturity^2)*x*DF[i] + TDF
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
      x <- (1.0 - exp(-LSCFC$Maturity/LSCFC$Tau1))/(LSCFC$Maturity/LSCFC$Tau1)
      TDF <- (LSCGC$Maturity^2)*x*DF[i] + TDF
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
      x <- (1.0 - exp(-LSCFC$Maturity/LSCFC$Tau1))/(LSCFC$Maturity/LSCFC$Tau1)
      TDF <- (LSCGC$Maturity^2)*x*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })
}
#
# Second derivative wrt FC slope and FC curvature 1
#
SDwrtFCSC1 = function(dfSWAP, dfHDInputs){
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
      x <- (1.0 - exp(-LSCFC$Maturity/LSCFC$Tau1))/(LSCFC$Maturity/LSCFC$Tau1)
      y <- (1.0 - exp(-LSCFC$Maturity/LSCFC$Tau2))/(LSCFC$Maturity/LSCFC$Tau2) -
        exp(-LSCFC$Maturity/LSCFC$Tau2)
      TDF <- (LSCFC$Maturity^2)*x*y*DF[i] + TDF
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
      x <- (1.0 - exp(-LSCFC$Maturity/LSCFC$Tau1))/(LSCFC$Maturity/LSCFC$Tau1)
      y <- (1.0 - exp(-LSCFC$Maturity/LSCFC$Tau2))/(LSCFC$Maturity/LSCFC$Tau2) -
        exp(-LSCFC$Maturity/LSCFC$Tau2)
      TDF <- (LSCFC$Maturity^2)*x*y*DF[i] + TDF
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
      x <- (1.0 - exp(-LSCFC$Maturity/LSCFC$Tau1))/(LSCFC$Maturity/LSCFC$Tau1)
      y <- (1.0 - exp(-LSCFC$Maturity/LSCFC$Tau2))/(LSCFC$Maturity/LSCFC$Tau2) -
        exp(-LSCFC$Maturity/LSCFC$Tau2)
      TDF <- (LSCFC$Maturity^2)*x*y*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })
}
#
# Second derivative wrt FC level and FC curvature 1
#
SDwrtFCLC1 = function(dfSWAP, dfHDInputs){
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
      TDF <- (LSCFC$Maturity^2)*x*DF[i] + TDF
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
      TDF <- (LSCFC$Maturity^2)*x*DF[i] + TDF
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
      TDF <- (LSCFC$Maturity^2)*x*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })
}
#
# Cross derivative wrt BC level and BC slope (NOT identical to FC level)
#
SDwrtBCLS = function(dfSWAP, dfHDInputs){
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
      x <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau1))/(LSCGC$Maturity/LSCGC$Tau1)
      TDF <- (LSCGC$Maturity^2)*x*DF[i] + TDF
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
      x <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau1))/(LSCGC$Maturity/LSCGC$Tau1)
      TDF <- (LSCGC$Maturity^2)*x*DF[i] + TDF
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
      x <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau1))/(LSCGC$Maturity/LSCGC$Tau1)
      TDF <- (LSCGC$Maturity^2)*x*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })
}
#
# Second derivative wrt BC slope and BC curvature 1
#
SDwrtBCSC1 = function(dfSWAP, dfHDInputs){
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
      x <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau1))/(LSCGC$Maturity/LSCGC$Tau1)
      y <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau2))/(LSCGC$Maturity/LSCGC$Tau2) - 
        exp(-LSCGC$Maturity/LSCGC$Tau2)
      TDF <- (LSCGC$Maturity^2)*x*y*DF[i] + TDF
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
      x <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau1))/(LSCGC$Maturity/LSCGC$Tau1)
      y <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau2))/(LSCGC$Maturity/LSCGC$Tau2) - 
        exp(-LSCGC$Maturity/LSCGC$Tau2)
      TDF <- (LSCGC$Maturity^2)*x*y*DF[i] + TDF
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
      x <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau1))/(LSCGC$Maturity/LSCGC$Tau1)
      y <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau2))/(LSCGC$Maturity/LSCGC$Tau2) - 
        exp(-LSCGC$Maturity/LSCGC$Tau2)
      TDF <- (LSCGC$Maturity^2)*x*y*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })
}



#
# Second derivative wrt BC level and BC curvature 1
#
SDwrtBCLC1 = function(dfSWAP, dfHDInputs){
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
      x <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau2))/(LSCGC$Maturity/LSCGC$Tau2) -
        exp(-LSCGC$Maturity/LSCGC$Tau2)
      TDF <- (LSCGC$Maturity^2)*x*DF[i] + TDF
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
      x <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau2))/(LSCGC$Maturity/LSCGC$Tau2) -
        exp(-LSCGC$Maturity/LSCGC$Tau2)
      TDF <- (LSCGC$Maturity^2)*x*DF[i] + TDF
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
      x <- (1.0 - exp(-LSCGC$Maturity/LSCGC$Tau2))/(LSCGC$Maturity/LSCGC$Tau2) -
        exp(-LSCGC$Maturity/LSCGC$Tau2)
      TDF <- (LSCGC$Maturity^2)*x*APFix[i]*DF[i] + TDF
    }
    TermFix <- NAmt*TDF
    SDSV <- dfSWAP$SwapType*(-TermFlt1+TermFlt2+(dfSWAP$FixedRate/100)*TermFix) 
    return( SDSV/10000.0 )
  })
}

