# Traditional Valuation Functions.R

# Compute Instrument Value Based on Given Set of traditional input parameters
TraditionalInstrumentValue = function(Trad){
  with(Trad, {
    Mat <- c(1:NumberOfYears)
    CF <- c(1:NumberOfYears)
    GR <- c(1:NumberOfYears)
    PVCF <- c(1:NumberOfYears)
    for (j in 1:NumberOfYears){
      if(j <= YearsInStage1){
        GR[j] <- g1
      } else {
        GR[j] <- g2
      }
      if(j == 1){
        CF[j] <- InitialCF * (1 + GR[j])
        PVCF[j] <- CF[j]/((1 + DR)^j)
      }
      if(j > 1){
        CF[j] <- CF[j-1] * (1 + GR[j])
        PVCF[j] <- CF[j]/((1 + DR)^j)
      } 
    }
    InstrumentValue <- sum(PVCF)
    return( InstrumentValue )
  })
}
# Implied perpetual growth rate
Impliedg2 <- function(Trad, inputInstrumentValue){
  TestFunctionImpliedg2<-function(testImpliedg2, Trad, inputInstrumentValue){
    Trad$g2 = testImpliedg2
    return( abs(inputInstrumentValue - TraditionalInstrumentValue(Trad))^2 )  
  }
  # NOTE: Tolerance set very low (alternative: .Machine$double.eps^0.25)
  solution = optimize(TestFunctionImpliedg2, Trad, inputInstrumentValue, 
    interval = c(Trad$ImpliedLowerBound, Trad$ImpliedUpperBound), 
    tol = .Machine$double.eps)
  Impliedg2 = solution$minimum
  Trad$g2 = Impliedg2
  Difference = inputInstrumentValue - TraditionalInstrumentValue(Trad)
  if (abs(Difference) < 0.01)return(Impliedg2)
  else return(NA)
}
# Implied perpetual growth rate
Impliedg1 <- function(Trad, inputInstrumentValue){
  TestFunctionImpliedg1<-function(testImpliedg1, Trad, inputInstrumentValue){
    Trad$g1 = testImpliedg1
    return( abs(inputInstrumentValue - TraditionalInstrumentValue(Trad))^2 )  
  }
  # NOTE: Tolerance set very low (alternative: .Machine$double.eps^0.25)
  solution = optimize(TestFunctionImpliedg1, Trad, inputInstrumentValue, 
    interval = c(Trad$ImpliedLowerBound, Trad$ImpliedUpperBound), 
    tol = .Machine$double.eps)
  Impliedg1 = solution$minimum
  Trad$g1 = Impliedg1
  Difference = inputInstrumentValue - TraditionalInstrumentValue(Trad)
  if (abs(Difference) < 0.01)return(Impliedg1)
  else return(NA)
}
# Compute instrument value based on given set of traditional input parameters
#  adjusted for health event (Moderate or Severe)
TraditionalInstrumentValueAdj = function(Trad, Decrement, Alpha){
  with(Trad, {
    Mat <- c(1:NumberOfYears)
    CF <- c(1:NumberOfYears)
    GR <- c(1:NumberOfYears)
    PVCF <- c(1:NumberOfYears)
    for (j in 1:NumberOfYears){
      if(j <= YearsInStage1){
        GR[j] <- g1 
      } else {
        GR[j] <- g2 
      }
      if(j == 1){
        CF[j] <- InitialCF * (1 + GR[j]) * (1 - Decrement[j])
        PVCF[j] <- CF[j]/(((1 + DR)*(1  + Alpha[j]))^j)
      }
      if(j > 1){
        CF[j] <- CF[j-1] * (1 + GR[j]) * (1 - Decrement[j])
        PVCF[j] <- CF[j]/(((1 + DR)*(1  + Alpha[j]))^j)
      } 
    }
    InstrumentValue <- sum(PVCF)
    return( InstrumentValue )
  })
}

