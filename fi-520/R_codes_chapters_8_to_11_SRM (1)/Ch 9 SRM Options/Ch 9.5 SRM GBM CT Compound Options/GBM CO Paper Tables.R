# GBM CO Greeks.R
# rmarkdown::render("GBM CO Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
# Libraries: pracma: integral2
Packages <- c("pracma", "mvtnorm", "pbivnorm", "openxlsx") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
par(family = 'Times New Roman') # Globally set fonts for graphs
#
# List expected as input in SpreadOptionValue
#
inputiC <- 1                              # 1-CO call, -1-CO put
inputiU <- 1                              # 1-UO call, -1-UO put
inputUnderlying <- 100.0                  # Must be positive
inputUnderlyingStrikePrice <- 100.0       # Must be positive
inputCompoundStrikePrice <- 5            # Must be positive
inputInterestRate <- log(1.1)*100                  # Must be positive
inputUnderlyingYield <- 0.0              # Payout of underlying instrument
inputOptionYield <- 0.0 # log(1.05)*100                    # Payout of option
inputVolatility <- 20.0                   # Must be positive
inputUnderlyingTimeToMaturity <- 1.0      # Must be positive
inputCompoundTimeToMaturity <- 0.25        # Must be positive
inputGreekIncrement = 0.001   # In percent of greek underlying variable

COInputData <- list(inputiC, inputiU, inputUnderlying, 
  inputUnderlyingStrikePrice, inputCompoundStrikePrice, inputInterestRate,
  inputUnderlyingYield, inputOptionYield, inputVolatility, 
  inputUnderlyingTimeToMaturity, inputCompoundTimeToMaturity,
  inputGreekIncrement) 
names(COInputData) <- c("iC", "iU", "S", "XU", "XC", "r", "d", "q", "v", 
  "TU", "TC", "Increment")
# source("BSMOVM and Extended Functions.R")
source("GBM COVM w Greeks Functions.R")
# Boundaries for critical stock price iterative search
LowerBound <- 0.0
UpperBound <- 2.0*COInputData$S #Critical stock price for put has 2 solutions
# Solve for implied option yield

CoCValues <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
CoCValues$Strike[1] <- 0.25
CoCValues$Strike[2] <- 0.375
CoCValues$Strike[3] <- 0.5
CoCValues$Strike[4] <- 0.625
CoCValues$Strike[5] <- 0.75

CoPValues <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
CoPValues$Strike[1] <- 0.25
CoPValues$Strike[2] <- 0.375
CoPValues$Strike[3] <- 0.5
CoPValues$Strike[4] <- 0.625
CoPValues$Strike[5] <- 0.75

PoCValues <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
PoCValues$Strike[1] <- 0.25
PoCValues$Strike[2] <- 0.375
PoCValues$Strike[3] <- 0.5
PoCValues$Strike[4] <- 0.625
PoCValues$Strike[5] <- 0.75

PoPValues <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
PoPValues$Strike[1] <- 0.25
PoPValues$Strike[2] <- 0.375
PoPValues$Strike[3] <- 0.5
PoPValues$Strike[4] <- 0.625
PoPValues$Strike[5] <- 0.75

CoCDeltas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
CoCDeltas$Strike[1] <- 0.25
CoCDeltas$Strike[2] <- 0.375
CoCDeltas$Strike[3] <- 0.5
CoCDeltas$Strike[4] <- 0.625
CoCDeltas$Strike[5] <- 0.75

CoPDeltas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
CoPDeltas$Strike[1] <- 0.25
CoPDeltas$Strike[2] <- 0.375
CoPDeltas$Strike[3] <- 0.5
CoPDeltas$Strike[4] <- 0.625
CoPDeltas$Strike[5] <- 0.75

PoCDeltas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
PoCDeltas$Strike[1] <- 0.25
PoCDeltas$Strike[2] <- 0.375
PoCDeltas$Strike[3] <- 0.5
PoCDeltas$Strike[4] <- 0.625
PoCDeltas$Strike[5] <- 0.75

PoPDeltas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
PoPDeltas$Strike[1] <- 0.25
PoPDeltas$Strike[2] <- 0.375
PoPDeltas$Strike[3] <- 0.5
PoPDeltas$Strike[4] <- 0.625
PoPDeltas$Strike[5] <- 0.75

CoCGammas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
CoCGammas$Strike[1] <- 0.25
CoCGammas$Strike[2] <- 0.375
CoCGammas$Strike[3] <- 0.5
CoCGammas$Strike[4] <- 0.625
CoCGammas$Strike[5] <- 0.75

CoPGammas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
CoPGammas$Strike[1] <- 0.25
CoPGammas$Strike[2] <- 0.375
CoPGammas$Strike[3] <- 0.5
CoPGammas$Strike[4] <- 0.625
CoPGammas$Strike[5] <- 0.75

PoCGammas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
PoCGammas$Strike[1] <- 0.25
PoCGammas$Strike[2] <- 0.375
PoCGammas$Strike[3] <- 0.5
PoCGammas$Strike[4] <- 0.625
PoCGammas$Strike[5] <- 0.75

PoPGammas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
PoPGammas$Strike[1] <- 0.25
PoPGammas$Strike[2] <- 0.375
PoPGammas$Strike[3] <- 0.5
PoPGammas$Strike[4] <- 0.625
PoPGammas$Strike[5] <- 0.75

CoCThetas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
CoCThetas$Strike[1] <- 0.25
CoCThetas$Strike[2] <- 0.375
CoCThetas$Strike[3] <- 0.5
CoCThetas$Strike[4] <- 0.625
CoCThetas$Strike[5] <- 0.75

CoPThetas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
CoPThetas$Strike[1] <- 0.25
CoPThetas$Strike[2] <- 0.375
CoPThetas$Strike[3] <- 0.5
CoPThetas$Strike[4] <- 0.625
CoPThetas$Strike[5] <- 0.75

PoCThetas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
PoCThetas$Strike[1] <- 0.25
PoCThetas$Strike[2] <- 0.375
PoCThetas$Strike[3] <- 0.5
PoCThetas$Strike[4] <- 0.625
PoCThetas$Strike[5] <- 0.75

PoPThetas <- data.frame(matrix(vector(), 5, 6,
  dimnames=list(c(), c("Strike", 5, 7.5, 10, 12.5, 15))), stringsAsFactors=F)
PoPThetas$Strike[1] <- 0.25
PoPThetas$Strike[2] <- 0.375
PoPThetas$Strike[3] <- 0.5
PoPThetas$Strike[4] <- 0.625
PoPThetas$Strike[5] <- 0.75

for(iType in 1:4){
  if(iType == 1){ # CoC
    COInputData$iC <- 1
    COInputData$iU <- 1
  }
  if(iType == 2){ # CoP
    COInputData$iC <- 1
    COInputData$iU <- -1
  }
  if(iType == 3){ # PoC
    COInputData$iC <- -1
    COInputData$iU <- 1
  }
  if(iType == 4){ # PoP
    COInputData$iC <- -1
    COInputData$iU <- -1
  }
  for(iTC in 1:5){
    if(iTC == 1) COInputData$TC <- 0.25
    if(iTC == 2) COInputData$TC <- 0.375
    if(iTC == 3) COInputData$TC <- 0.5
    if(iTC == 4) COInputData$TC <- 0.625
    if(iTC == 5) COInputData$TC <- 0.75
    for(iXC in 1:5){
      if(iXC == 1) COInputData$XC <- 5
      if(iXC == 2) COInputData$XC <- 7.5
      if(iXC == 3) COInputData$XC <- 10
      if(iXC == 4) COInputData$XC <- 12.5
      if(iXC == 5) COInputData$XC <- 15
      if(iType == 1){ # CoC
        CoCValues[iTC,iXC+1] <- round(COValue(COInputData, LowerBound, UpperBound),2)
        CoCDeltas[iTC,iXC+1] <- round(CODelta(COInputData, LowerBound, UpperBound),2)
        CoCGammas[iTC,iXC+1] <- round(COGamma(COInputData, LowerBound, UpperBound),3)
        CoCThetas[iTC,iXC+1] <- round(COTheta(COInputData, LowerBound, UpperBound),2)
      }
      if(iType == 2){ # CoP
        CoPValues[iTC,iXC+1] <- round(COValue(COInputData, LowerBound, UpperBound),2)
        CoPDeltas[iTC,iXC+1] <- round(CODelta(COInputData, LowerBound, UpperBound),2)
        CoPGammas[iTC,iXC+1] <- round(COGamma(COInputData, LowerBound, UpperBound),3)
        CoPThetas[iTC,iXC+1] <- round(COTheta(COInputData, LowerBound, UpperBound),2)
      }
      if(iType == 3){ # PoC
        PoCValues[iTC,iXC+1] <- round(COValue(COInputData, LowerBound, UpperBound),2)
        PoCDeltas[iTC,iXC+1] <- round(CODelta(COInputData, LowerBound, UpperBound),2)
        PoCGammas[iTC,iXC+1] <- round(COGamma(COInputData, LowerBound, UpperBound),3)
        PoCThetas[iTC,iXC+1] <- round(COTheta(COInputData, LowerBound, UpperBound),2)
      }
      if(iType == 4){ # PoP
        PoPValues[iTC,iXC+1] <- round(COValue(COInputData, LowerBound, UpperBound),2)
        PoPDeltas[iTC,iXC+1] <- round(CODelta(COInputData, LowerBound, UpperBound),2)
        PoPGammas[iTC,iXC+1] <- round(COGamma(COInputData, LowerBound, UpperBound),3)
        PoPThetas[iTC,iXC+1] <- round(COTheta(COInputData, LowerBound, UpperBound),2)
      }      
    }
  }
}

names(CoCValues)[names(CoCValues) == 'X5'] <-    'CoC  5.0'
names(CoCValues)[names(CoCValues) == 'X7.5'] <-  'CoC  7.5'
names(CoCValues)[names(CoCValues) == 'X10'] <-   'CoC 10.0'
names(CoCValues)[names(CoCValues) == 'X12.5'] <- 'CoC 12.5'
names(CoCValues)[names(CoCValues) == 'X15'] <-   'CoC 15.0'
names(CoPValues)[names(CoPValues) == 'X5'] <-    'CoP  5.0'
names(CoPValues)[names(CoPValues) == 'X7.5'] <-  'CoP  7.5'
names(CoPValues)[names(CoPValues) == 'X10'] <-   'CoP 10.0'
names(CoPValues)[names(CoPValues) == 'X12.5'] <- 'CoP 12.5'
names(CoPValues)[names(CoPValues) == 'X15'] <-   'CoP 15.0'
names(PoCValues)[names(PoCValues) == 'X5'] <-    'PoC  5.0'
names(PoCValues)[names(PoCValues) == 'X7.5'] <-  'PoC  7.5'
names(PoCValues)[names(PoCValues) == 'X10'] <-   'PoC 10.0'
names(PoCValues)[names(PoCValues) == 'X12.5'] <- 'PoC 12.5'
names(PoCValues)[names(PoCValues) == 'X15'] <-   'PoC 15.0'
names(PoPValues)[names(PoPValues) == 'X5'] <-    'PoP  5.0'
names(PoPValues)[names(PoPValues) == 'X7.5'] <-  'PoP  7.5'
names(PoPValues)[names(PoPValues) == 'X10'] <-   'PoP 10.0'
names(PoPValues)[names(PoPValues) == 'X12.5'] <- 'PoP 12.5'
names(PoPValues)[names(PoPValues) == 'X15'] <-   'PoP 15.0'

names(CoCDeltas)[names(CoCDeltas) == 'X5'] <-    'CoC  5.0'
names(CoCDeltas)[names(CoCDeltas) == 'X7.5'] <-  'CoC  7.5'
names(CoCDeltas)[names(CoCDeltas) == 'X10'] <-   'CoC 10.0'
names(CoCDeltas)[names(CoCDeltas) == 'X12.5'] <- 'CoC 12.5'
names(CoCDeltas)[names(CoCDeltas) == 'X15'] <-   'CoC 15.0'
names(CoPDeltas)[names(CoPDeltas) == 'X5'] <-    'CoP  5.0'
names(CoPDeltas)[names(CoPDeltas) == 'X7.5'] <-  'CoP  7.5'
names(CoPDeltas)[names(CoPDeltas) == 'X10'] <-   'CoP 10.0'
names(CoPDeltas)[names(CoPDeltas) == 'X12.5'] <- 'CoP 12.5'
names(CoPDeltas)[names(CoPDeltas) == 'X15'] <-   'CoP 15.0'
names(PoCDeltas)[names(PoCDeltas) == 'X5'] <-    'PoC  5.0'
names(PoCDeltas)[names(PoCDeltas) == 'X7.5'] <-  'PoC  7.5'
names(PoCDeltas)[names(PoCDeltas) == 'X10'] <-   'PoC 10.0'
names(PoCDeltas)[names(PoCDeltas) == 'X12.5'] <- 'PoC 12.5'
names(PoCDeltas)[names(PoCDeltas) == 'X15'] <-   'PoC 15.0'
names(PoPDeltas)[names(PoPDeltas) == 'X5'] <-    'PoP  5.0'
names(PoPDeltas)[names(PoPDeltas) == 'X7.5'] <-  'PoP  7.5'
names(PoPDeltas)[names(PoPDeltas) == 'X10'] <-   'PoP 10.0'
names(PoPDeltas)[names(PoPDeltas) == 'X12.5'] <- 'PoP 12.5'
names(PoPDeltas)[names(PoPDeltas) == 'X15'] <-   'PoP 15.0'

names(CoCGammas)[names(CoCGammas) == 'X5'] <-    'CoC  5.0'
names(CoCGammas)[names(CoCGammas) == 'X7.5'] <-  'CoC  7.5'
names(CoCGammas)[names(CoCGammas) == 'X10'] <-   'CoC 10.0'
names(CoCGammas)[names(CoCGammas) == 'X12.5'] <- 'CoC 12.5'
names(CoCGammas)[names(CoCGammas) == 'X15'] <-   'CoC 15.0'
names(CoPGammas)[names(CoPGammas) == 'X5'] <-    'CoP  5.0'
names(CoPGammas)[names(CoPGammas) == 'X7.5'] <-  'CoP  7.5'
names(CoPGammas)[names(CoPGammas) == 'X10'] <-   'CoP 10.0'
names(CoPGammas)[names(CoPGammas) == 'X12.5'] <- 'CoP 12.5'
names(CoPGammas)[names(CoPGammas) == 'X15'] <-   'CoP 15.0'
names(PoCGammas)[names(PoCGammas) == 'X5'] <-    'PoC  5.0'
names(PoCGammas)[names(PoCGammas) == 'X7.5'] <-  'PoC  7.5'
names(PoCGammas)[names(PoCGammas) == 'X10'] <-   'PoC 10.0'
names(PoCGammas)[names(PoCGammas) == 'X12.5'] <- 'PoC 12.5'
names(PoCGammas)[names(PoCGammas) == 'X15'] <-   'PoC 15.0'
names(PoPGammas)[names(PoPGammas) == 'X5'] <-    'PoP  5.0'
names(PoPGammas)[names(PoPGammas) == 'X7.5'] <-  'PoP  7.5'
names(PoPGammas)[names(PoPGammas) == 'X10'] <-   'PoP 10.0'
names(PoPGammas)[names(PoPGammas) == 'X12.5'] <- 'PoP 12.5'
names(PoPGammas)[names(PoPGammas) == 'X15'] <-   'PoP 15.0'

names(CoCThetas)[names(CoCThetas) == 'X5'] <-    'CoC  5.0'
names(CoCThetas)[names(CoCThetas) == 'X7.5'] <-  'CoC  7.5'
names(CoCThetas)[names(CoCThetas) == 'X10'] <-   'CoC 10.0'
names(CoCThetas)[names(CoCThetas) == 'X12.5'] <- 'CoC 12.5'
names(CoCThetas)[names(CoCThetas) == 'X15'] <-   'CoC 15.0'
names(CoPThetas)[names(CoPThetas) == 'X5'] <-    'CoP  5.0'
names(CoPThetas)[names(CoPThetas) == 'X7.5'] <-  'CoP  7.5'
names(CoPThetas)[names(CoPThetas) == 'X10'] <-   'CoP 10.0'
names(CoPThetas)[names(CoPThetas) == 'X12.5'] <- 'CoP 12.5'
names(CoPThetas)[names(CoPThetas) == 'X15'] <-   'CoP 15.0'
names(PoCThetas)[names(PoCThetas) == 'X5'] <-    'PoC  5.0'
names(PoCThetas)[names(PoCThetas) == 'X7.5'] <-  'PoC  7.5'
names(PoCThetas)[names(PoCThetas) == 'X10'] <-   'PoC 10.0'
names(PoCThetas)[names(PoCThetas) == 'X12.5'] <- 'PoC 12.5'
names(PoCThetas)[names(PoCThetas) == 'X15'] <-   'PoC 15.0'
names(PoPThetas)[names(PoPThetas) == 'X5'] <-    'PoP  5.0'
names(PoPThetas)[names(PoPThetas) == 'X7.5'] <-  'PoP  7.5'
names(PoPThetas)[names(PoPThetas) == 'X10'] <-   'PoP 10.0'
names(PoPThetas)[names(PoPThetas) == 'X12.5'] <- 'PoP 12.5'
names(PoPThetas)[names(PoPThetas) == 'X15'] <-   'PoP 15.0'


Values <- merge(x = CoCValues, y = CoPValues, by = "Strike", sort = TRUE) 
Values <- merge(x = Values, y = PoCValues, by = "Strike", sort = TRUE) 
Values <- merge(x = Values, y = PoPValues, by = "Strike", sort = TRUE) 

Deltas <- merge(x = CoCDeltas, y = CoPDeltas, by = "Strike", sort = TRUE) 
Deltas <- merge(x = Deltas, y = PoCDeltas, by = "Strike", sort = TRUE) 
Deltas <- merge(x = Deltas, y = PoPDeltas, by = "Strike", sort = TRUE) 

Gammas <- merge(x = CoCGammas, y = CoPGammas, by = "Strike", sort = TRUE) 
Gammas <- merge(x = Gammas, y = PoCGammas, by = "Strike", sort = TRUE) 
Gammas <- merge(x = Gammas, y = PoPGammas, by = "Strike", sort = TRUE) 

Thetas <- merge(x = CoCThetas, y = CoPThetas, by = "Strike", sort = TRUE) 
Thetas <- merge(x = Thetas, y = PoCThetas, by = "Strike", sort = TRUE) 
Thetas <- merge(x = Thetas, y = PoPThetas, by = "Strike", sort = TRUE) 

if(inputUnderlyingYield > 0){
  CORubinsteinReplValues <- t(Values)
  CORubinsteinReplDeltas <- t(Deltas)
  CORubinsteinReplGammas <- t(Gammas)
  CORubinsteinReplThetas <- t(Thetas)
  write.xlsx(CORubinsteinReplValues, "Rubinstein Values.xlsx", 
    sheetName = "Values", col.names=TRUE, row.names=TRUE, append=FALSE, 
    showNA=TRUE)
  write.xlsx(CORubinsteinReplDeltas, "Rubinstein Deltas.xlsx", 
    sheetName = "Deltas", col.names=TRUE, row.names=TRUE, append=FALSE, 
    showNA=TRUE)
  write.xlsx(CORubinsteinReplGammas, "Rubinstein Gammas.xlsx", 
    sheetName = "Gammas", col.names=TRUE, row.names=TRUE, append=FALSE, 
    showNA=TRUE)
  write.xlsx(CORubinsteinReplThetas, "Rubinstein Thetas.xlsx", 
    sheetName = "Thetas", col.names=TRUE, row.names=TRUE, append=FALSE, 
    showNA=TRUE)
  CORubinsteinReplValues
  CORubinsteinReplDeltas
  CORubinsteinReplGammas
  CORubinsteinReplThetas
} else {
  COOptionYieldValues <- t(Values)
  COOptionYieldDeltas <- t(Deltas)
  COOptionYieldGammas <- t(Gammas)
  COOptionYieldThetas <- t(Thetas)
  write.xlsx(COOptionYieldValues, "Option Yield Values.xlsx", 
    sheetName = "Values", col.names=TRUE, row.names=TRUE, append=FALSE, 
    showNA=TRUE)
  write.xlsx(COOptionYieldDeltas, "Option Yield Deltas.xlsx", 
    sheetName = "Deltas", col.names=TRUE, row.names=TRUE, append=FALSE, 
    showNA=TRUE)
  write.xlsx(COOptionYieldGammas, "Option Yield Gammas.xlsx",
    sheetName = "Gammas", col.names=TRUE, row.names=TRUE, append=FALSE,
    showNA=TRUE)
  write.xlsx(COOptionYieldThetas, "Option Yield Thetas.xlsx",
    sheetName = "Thetas", col.names=TRUE, row.names=TRUE, append=FALSE,
    showNA=TRUE)
  COOptionYieldValues
  COOptionYieldDeltas
  COOptionYieldGammas
  COOptionYieldThetas
}

t(CoCDeltas)
t(CoPDeltas)
t(PoCDeltas)
t(PoPDeltas)

t(CoCGammas)
t(CoPGammas)
t(PoCGammas)
t(PoPGammas)

t(CoCThetas)
t(CoPThetas)
t(PoCThetas)
t(PoPThetas)

# # Challenging thought experiment
# 
# inputUnderlying <- COInputData$S
# inputUnderlyingStrikePrice <- COInputData$XU
# inputInterestRate <- COInputData$r
# inputUnderlyingYield <- COInputData$d
# inputOptionYield <- COInputData$q
# inputVolatility <- COInputData$v
# inputTimeToMaturity <- COInputData$TU
# inputType <- COInputData$iU
# BSMInputData <- list(inputUnderlying, inputUnderlyingStrikePrice, 
#   inputInterestRate, inputUnderlyingYield,  inputOptionYield, 
#   inputVolatility, inputTimeToMaturity, inputType)
# names(BSMInputData) <- c("StockPrice", "StrikePrice", "InterestRate",
#   "DividendYield", "OptionYield", "Volatility", "TimeToMaturity", "Type")
# UCallValue <- BSMOptionValue(BSMInputData)
# BSMInputData$Type <- -1
# UPutValue <- BSMOptionValue(BSMInputData)
# UCallValue; UPutValue
# 
# # Base case
# COInputData$iC <- 1
# COInputData$iU <- 1
# CoCCSP <- COCriticalStockPrice(COInputData, UCallValue, LowerBound, UpperBound)
# CoC <- COValue(COInputData, LowerBound, UpperBound)
# CoCNGDelta <- CONGDelta(COInputData, LowerBound, UpperBound)
# CoCNGGamma <- CONGGamma(COInputData, LowerBound, UpperBound)
# CoCNGTheta <- CONGTheta(COInputData, LowerBound, UpperBound)
# COInputData$iU <- -1
# CoPCSP <- COCriticalStockPrice(COInputData, UPutValue, LowerBound, UpperBound)
# CoP <- COValue(COInputData, LowerBound, UpperBound)
# CoPNGDelta <- CONGDelta(COInputData, LowerBound, UpperBound)
# CoPNGGamma <- CONGGamma(COInputData, LowerBound, UpperBound)
# CoPNGTheta <- CONGTheta(COInputData, LowerBound, UpperBound)
# COInputData$iC <- -1
# COInputData$iU <- 1
# PoCCSP <- COCriticalStockPrice(COInputData, UCallValue, LowerBound, UpperBound)
# PoC <- COValue(COInputData, LowerBound, UpperBound)
# PoCNGDelta <- CONGDelta(COInputData, LowerBound, UpperBound)
# PoCNGGamma <- CONGGamma(COInputData, LowerBound, UpperBound)
# PoCNGTheta <- CONGTheta(COInputData, LowerBound, UpperBound)
# COInputData$iU <- -1
# PoPCSP <- COCriticalStockPrice(COInputData, UPutValue, LowerBound, UpperBound)
# PoP <- COValue(COInputData, LowerBound, UpperBound)
# PoPNGDelta <- CONGDelta(COInputData, LowerBound, UpperBound)
# PoPNGGamma <- CONGGamma(COInputData, LowerBound, UpperBound)
# PoPNGTheta <- CONGTheta(COInputData, LowerBound, UpperBound)
# CoC; CoP; PoC; PoP
# CoCNGDelta; CoPNGDelta; PoCNGDelta; PoPNGDelta
# CoCNGGamma; CoPNGGamma; PoCNGGamma; PoPNGGamma
# # Lower Bounds
# COInputData$iC <- 1
# COInputData$iU <- 1
# CoCLB <- COLowerBound(COInputData)
# COInputData$iC <- 1
# COInputData$iU <- -1
# CoPLB <- COLowerBound(COInputData)
# COInputData$iC <- -1
# COInputData$iU <- 1
# PoCLB <- COLowerBound(COInputData)
# COInputData$iC <- -1
# COInputData$iU <- -1
# PoPLB <- COLowerBound(COInputData)
# CoCLB; CoPLB; PoCLB; PoPLB
# # Upper Bounds
# COInputData$iC <- 1
# COInputData$iU <- 1
# CoCUB <- COUpperBound(COInputData)
# COInputData$iC <- 1
# COInputData$iU <- -1
# CoPUB <- COUpperBound(COInputData)
# COInputData$iC <- -1
# COInputData$iU <- 1
# PoCUB <- COUpperBound(COInputData)
# COInputData$iC <- -1
# COInputData$iU <- -1
# PoPUB <- COUpperBound(COInputData)
# CoCUB; CoPUB; PoCUB; PoPUB
# # Delta
# COInputData$iC <- 1
# COInputData$iU <- 1
# CoCDelta <- CODelta(COInputData, LowerBound, UpperBound)
# COInputData$iC <- 1
# COInputData$iU <- -1
# CoPDelta <- CODelta(COInputData, LowerBound, UpperBound)
# COInputData$iC <- -1
# COInputData$iU <- 1
# PoCDelta <- CODelta(COInputData, LowerBound, UpperBound)
# COInputData$iC <- -1
# COInputData$iU <- -1
# PoPDelta <- CODelta(COInputData, LowerBound, UpperBound)
# # Gamma
# COInputData$iC <- 1
# COInputData$iU <- 1
# CoCGamma <- COGamma(COInputData, LowerBound, UpperBound)
# COInputData$iC <- 1
# COInputData$iU <- -1
# CoPGamma <- COGamma(COInputData, LowerBound, UpperBound)
# COInputData$iC <- -1
# COInputData$iU <- 1
# PoCGamma <- COGamma(COInputData, LowerBound, UpperBound)
# COInputData$iC <- -1
# COInputData$iU <- -1
# PoPGamma <- COGamma(COInputData, LowerBound, UpperBound)
# # Theta
# COInputData$iC <- 1
# COInputData$iU <- 1
# CoCTheta <- COTheta(COInputData, LowerBound, UpperBound)
# COInputData$iC <- 1
# COInputData$iU <- -1
# CoPTheta <- COTheta(COInputData, LowerBound, UpperBound)
# COInputData$iC <- -1
# COInputData$iU <- 1
# PoCTheta <- COTheta(COInputData, LowerBound, UpperBound)
# COInputData$iC <- -1
# COInputData$iU <- -1
# PoPTheta <- COTheta(COInputData, LowerBound, UpperBound)
# 
# CoC; CoP; PoC; PoP
# CoCLB; CoPLB; PoCLB; PoPLB
# CoCUB; CoPUB; PoCUB; PoPUB
# CoCCSP; CoPCSP; PoCCSP; PoPCSP
# CoCDelta; CoPDelta; PoCDelta; PoPDelta
# CoCNGDelta; CoPNGDelta; PoCNGDelta; PoPNGDelta
# CoCGamma; CoPGamma; PoCGamma; PoPGamma
# CoCNGGamma; CoPNGGamma; PoCNGGamma; PoPNGGamma
# 
# xCoC <- (COInputData$r/100)*CoC - 
#   (COInputData$r/100 - COInputData$q/100)*CoCDelta*COInputData$S -
#   0.5*(COInputData$v/100)^2*COInputData$S*CoCGamma
# xCoP <- (COInputData$r/100)*CoP - 
#   (COInputData$r/100 - COInputData$q/100)*CoPDelta*COInputData$S -
#   0.5*(COInputData$v/100)^2*COInputData$S*CoPGamma
# xPoC <- (COInputData$r/100)*PoC - 
#   (COInputData$r/100 - COInputData$q/100)*PoCDelta*COInputData$S -
#   0.5*(COInputData$v/100)^2*COInputData$S*PoCGamma
# xPoP <- (COInputData$r/100)*PoP - 
#   (COInputData$r/100 - COInputData$q/100)*PoPDelta*COInputData$S -
#   0.5*(COInputData$v/100)^2*COInputData$S*PoPGamma
# 
# CoCTheta; CoPTheta; PoCTheta; PoPTheta
# CoCNGTheta; CoPNGTheta; PoCNGTheta; PoPNGTheta
# xCoC; xCoP; xPoC; xPoP
# 
# CoC; CoP; PoC; PoP

