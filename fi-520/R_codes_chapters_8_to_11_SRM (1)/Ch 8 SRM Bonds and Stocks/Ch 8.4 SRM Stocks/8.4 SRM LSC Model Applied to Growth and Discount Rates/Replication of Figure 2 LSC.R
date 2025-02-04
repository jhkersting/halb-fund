# Replication of Figure 2.R
#
# Eventually place in separate source file
#
Decrement <- Scenario$Growth[1]
Alpha <- Scenario$WACC[1]

source("LSC Model Functions.R")
inputInstrumentValue <- -99
inputGLevel = -99 
inputGSlope = -99 # Placeholer
inputGCurve1 = 0
inputWLevel = -99 
inputWSlope = -99 
inputWCurve1 = 0
inputScalarG = ScalarG
inputScalarF = ScalarF
inputNumberOfYears <- NumberOfMaturities
inputWACC <- -99 # Placeholder
inputInitialCF <- 1.0
inputImpliedLowerBound <- -10
inputImpliedUpperBound <- 10
#  Input matrix
#  LSCInputData - eventually generalize
LSCModelInputs <- list(inputGLevel, inputGSlope, inputGCurve1, 
  inputWLevel, inputWSlope, inputWCurve1, 
  inputScalarG, inputScalarF, 
  inputNumberOfYears, inputWACC, inputInitialCF, 
  inputImpliedLowerBound, inputImpliedUpperBound)
names(LSCModelInputs) <- c("LSCGLevel", "LSCGSlope", "LSCGCurve1",
  "LSCWLevel", "LSCWSlope", "LSCWCurve1",
  "LSCScalarG", "LSCScalarF", 
  "LSCNumberOfYears", "LSCWACC", "LSCInitialCF", 
  "ImpliedLowerBound", "ImpliedUpperBound")


#
# Focus is on:
#  China = Quintile 4
#  India = Quintile 3
#  Nigeria = Quintile 2
#
# PE is at individual industry level (II and CF0Q2/3/4M2)
#
CF0QXV <- CF0QX # Build value file
CF0QXVAdj <- CF0QX # Build value adjusted file
#
# Need to eliminate Manufacturing due to lack of PE ratio in II
#
keepRow <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
             "11", "12", "13", "14", "15")
CF0QXV <- CF0QXV[keepRow,] 
CF0QXVAdj <- CF0QXVAdj[keepRow,] 
CF0QXV$TotalValueChk <- 0
CF0QXVAdj$TotalValueChk <- 0
ncolQ2 <- ncol(CF0QXV)
nrowQ2 <- nrow(CF0QXV)
for(j in 2:(ncolQ2 - 3)){
  for(i in 1:nrowQ2){
    CF0QXV[i,j] <- 0.0
    CF0QXVAdj[i,j] <- 0.0
    LSCModelInputs$LSCGLevel = II$GLevel[i]/100 
    LSCModelInputs$LSCGSlope = II$GSlope[i]/100 
    LSCModelInputs$LSCWLevel = II$WLevel[i]/100
    LSCModelInputs$LSCWSlope <- II$WSlope[i]/100
    CF0QXV[i,j] <- as.numeric(LSCInstrumentValueGW(LSCModelInputs)) *
      as.numeric(CF0QX[i,j])
    CF0QXV$TotalValueChk[i] <-  CF0QXV$TotalValueChk[i] + 
      as.numeric(CF0QXV[i,j])
    CF0QXVAdj[i,j] <- as.numeric(LSCInstrumentValueGWAdj(LSCModelInputs, Decrement, Alpha)) *
      as.numeric(CF0QX[i,j])
    CF0QXVAdj$TotalValueChk[i] <-  CF0QXVAdj$TotalValueChk[i] + 
      as.numeric(CF0QXVAdj[i,j])
  }
}
#
# Compress results for Manufacturing
#
CF0QXV[nrow(CF0QXV) + 1,] = 1 #nrow: Number of rows
CF0QXVAdj[nrow(CF0QXVAdj) + 1,] = 1 #nrow: Number of rows
CF0QXV[nrow(CF0QXV),1] <- "Manufacturing"
CF0QXVAdj[nrow(CF0QXVAdj),1] <- "Manufacturing"
for(i in 2:ncol(CF0QXV)){
  CF0QXV[nrow(CF0QXV),i] <- as.numeric(CF0QXV[2,i]) +
    + as.numeric(CF0QXV[3,i]) + as.numeric(CF0QXV[4,i]) +
    + as.numeric(CF0QXV[5,i]) + as.numeric(CF0QXV[6,i]) +
    + as.numeric(CF0QXV[7,i]) + as.numeric(CF0QXV[8,i]) +
    + as.numeric(CF0QXV[9,i])
  CF0QXVAdj[nrow(CF0QXVAdj),i] <- as.numeric(CF0QXVAdj[2,i]) +
    + as.numeric(CF0QXVAdj[3,i]) + as.numeric(CF0QXVAdj[4,i]) +
    + as.numeric(CF0QXVAdj[5,i]) + as.numeric(CF0QXVAdj[6,i]) +
    + as.numeric(CF0QXVAdj[7,i]) + as.numeric(CF0QXVAdj[8,i]) +
    + as.numeric(CF0QXVAdj[9,i])
}
keepRow <- c("1", "10", "11", "12", "13", "14", "15", "16")
CF0QXV <- CF0QXV[keepRow,] 
CF0QXVAdj <- CF0QXVAdj[keepRow,] 
LastRow <- nrow(CF0QXV)
LastCol <- ncol(CF0QXV)
CF0QXV[LastRow+1,1] <- "Total"
CF0QXVAdj[LastRow+1,1] <- "Total"
for(i in 2:LastCol){
  CF0QXV[LastRow+1,i]  <- 0.0 # Delete NAs
  CF0QXVAdj[LastRow+1,i]  <- 0.0 
  CF0QXV[LastRow+1,i]  <- sum(as.numeric(CF0QXV[,i]))
  CF0QXVAdj[LastRow+1,i]  <- sum(as.numeric(CF0QXVAdj[,i]))
}
#
# Work on change file
#
CF0QXVChg <- CF0QXVAdj
CF0QXPChg <- CF0QXVAdj
NRow <- nrow(CF0QXV)
NCol <- ncol(CF0QXV)
for(i in 2:(NCol-3)){
  for(j in 1:NRow){
    CF0QXVChg[j,i] <- as.numeric(CF0QXV[j,i]) - as.numeric(CF0QXVAdj[j,i])
    CF0QXPChg[j,i] <- 100.0*(1 - ( as.numeric(CF0QXVAdj[j,i])/as.numeric(CF0QXV[j,i]) ))
    CF0QXV[j,i] <- as.numeric(as.character(CF0QXV[j,i]))
    CF0QXVChg[j,i] <- as.numeric(as.character(CF0QXVChg[j,i]))
    CF0QXPChg[j,i] <- as.numeric(as.character(CF0QXPChg[j,i]))
  }
}
for(j in 1:NRow){
  CF0QXVChg[j,NCol] <- as.numeric(CF0QXV[j,NCol]) - as.numeric(CF0QXVAdj[j,NCol])
  CF0QXPChg[j,NCol] <- 100.0*(1 - ( as.numeric(CF0QXVAdj[j,NCol])/as.numeric(CF0QXV[j,NCol]) ))
  CF0QXV[j,NCol] <- as.numeric(as.character(CF0QXV[j,NCol]))
  CF0QXVChg[j,NCol] <- as.numeric(as.character(CF0QXVChg[j,NCol]))
  CF0QXPChg[j,NCol] <- as.numeric(as.character(CF0QXPChg[j,NCol]))
}
names(CF0QXVChg)[names(CF0QXVChg)=="TotalValueChk"] <- "TotalValueChg"
names(CF0QXPChg)[names(CF0QXPChg)=="TotalValueChk"] <- "TotalValuePChg"
Industry <- CF0QXV$Industry
CF0QXV <- data.frame(apply(CF0QXV, 2, function(x) as.numeric(as.character(x))))
CF0QXV <- CF0QXV %>% mutate_if(is.numeric, round, digits = 4)
CF0QXV$Industry <- Industry
CF0QXVChg <- data.frame(apply(CF0QXVChg, 2, function(x) as.numeric(as.character(x))))
CF0QXVChg <- CF0QXVChg %>% mutate_if(is.numeric, round, digits = 4)
CF0QXVChg$Industry <- Industry
CF0QXPChg <- data.frame(apply(CF0QXPChg, 2, function(x) as.numeric(as.character(x))))
CF0QXPChg <- CF0QXPChg %>% mutate_if(is.numeric, round, digits = 4)
CF0QXPChg$Industry <- Industry