# GBM Sensitivity Analysis.R
# rmarkdown::render("GBM CO Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
# Libraries: pracma: integral2
Packages <- c("pracma", "mvtnorm", "pbivnorm") 
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
inputCompoundStrikePrice <- 18            # Must be positive
inputInterestRate <- 5.0                  # Must be positive
inputUnderlyingYield <- 3.0              # Payout of underlying instrument
inputOptionYield <- 2.0                    # Payout of option
inputVolatility <- 30.0                   # Must be positive
inputUnderlyingTimeToMaturity <- 2.0      # Must be positive
inputCompoundTimeToMaturity <- 1.0        # Must be positive
inputGreekIncrement = 0.001   # In percent of greek underlying variable

COInputData <- list(inputiC, inputiU, inputUnderlying, 
  inputUnderlyingStrikePrice, inputCompoundStrikePrice, inputInterestRate,
  inputUnderlyingYield, inputOptionYield, inputVolatility, 
  inputUnderlyingTimeToMaturity, inputCompoundTimeToMaturity,
  inputGreekIncrement) 
names(COInputData) <- c("iC", "iU", "S", "XU", "XC", "r", "d", "q", "v", 
  "TU", "TC", "Increment")

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
Increment <- 0.75
SLowerBound <- COInputData$S*(1 - Increment)
SUpperBound <- COInputData$S*(1 + Increment)
VIncrement <- 0.75
VLowerBound <- COInputData$v*(1 - VIncrement)
VUpperBound <- COInputData$v*(1 + VIncrement)
qIncrement <- 0.75
qLowerBound <- 0.0
qUpperBound <- 10.0
# tIncrement <- 
tLowerBound <- 0.0
tUpperBound <- COInputData$TC - 0.05
source("GBM CO Sensitivity Analysis wrt Stock Price.R")
# source("GBM CO Sensitivity Analysis wrt Volatility.R")
# source("GBM CO Sensitivity Analysis wrt Option Yield.R")
# source("GBM CO Sensitivity Analysis wrt Maturity.R")
