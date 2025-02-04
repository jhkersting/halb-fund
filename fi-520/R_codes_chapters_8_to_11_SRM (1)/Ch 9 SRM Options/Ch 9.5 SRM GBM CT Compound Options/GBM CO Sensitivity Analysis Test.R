# GBM CO Sensitivity Analysis Test.R
# rmarkdown::render("GBM CO Sensitivity Analysis Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
# Libraries: pracma: integral2
Packages <- c("pracma", "mvtnorm", "pbivnorm", "beepr") 
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
inputCompoundStrikePrice <- 20.46         # Must be positive
inputInterestRate <- 5.0                  # Must be positive
inputUnderlyingYield <- 5.0               # Payout of underlying instrument
inputOptionYield <- 0.0                   # Payout of option
inputVolatility <- 30.0                   # Must be positive
inputUnderlyingTimeToMaturity <- 5.0      # Must be positive
inputCompoundTimeToMaturity <- 1.0        # Must be positive
inputGreekIncrement = 0.001   # In percent of greek underlying variable
# Compound option
COInputData <- list(inputiC, inputiU, inputUnderlying, 
  inputUnderlyingStrikePrice, inputCompoundStrikePrice, inputInterestRate,
  inputUnderlyingYield, inputOptionYield, inputVolatility, 
  inputUnderlyingTimeToMaturity, inputCompoundTimeToMaturity,
  inputGreekIncrement) 
names(COInputData) <- c("iC", "iU", "S", "XU", "XC", "r", "d", "q", "v", 
  "TU", "TC", "Increment")
# Underlying option
inputTimeToMaturity <- inputUnderlyingTimeToMaturity
inputType <- inputiU
BSMInputData <- list(inputUnderlying, inputUnderlyingStrikePrice, 
  inputInterestRate, inputUnderlyingYield, inputOptionYield, 
  inputVolatility, inputTimeToMaturity, inputType)
names(BSMInputData) <- c("StockPrice", "StrikePrice", "InterestRate",
  "DividendYield", "OptionYield", "Volatility", "TimeToMaturity", "Type")
source("GBM COVM w Greeks Functions.R")
# Find at-the-money strike price
BSMInputData$Type <- 1
UCallOption <- BSMOptionValue(BSMInputData)
BSMInputData$Type <- -1
UPutOption <- BSMOptionValue(BSMInputData)
UCallOption; UPutOption
# Boundaries for critical stock price iterative search
LowerBound <- 0.0
UpperBound <- 2.0*COInputData$S #Critical stock price for put has 2 solutions
# Solve for implied option yield
NumberOfObservations <- 101
# Underlying instrument range (stock price)
Increment <- 2.0
# SLowerBound <- COInputData$S*(1 - Increment)
SLowerBound <- 0.05
SUpperBound <- COInputData$S*(1 + Increment)
# Volatility range
VIncrement <- 0.5
VLowerBound <- COInputData$v*(1 - VIncrement)
VUpperBound <- COInputData$v*(1 + VIncrement)
# Option yield range
qIncrement <- 0.75
qLowerBound <- 0.0
qUpperBound <- 10.0
# Calendar time range
tLowerBound <- 0.0
tUpperBound <- COInputData$TC - 0.05
source("GBM CO Sensitivity Analysis wrt Stock Price.R")
source("GBM CO Sensitivity Analysis wrt Volatility.R")
source("GBM CO Sensitivity Analysis wrt Option Yield.R")
source("GBM CO Sensitivity Analysis wrt Maturity.R")

beep(sound = 2, print('Finished'))

