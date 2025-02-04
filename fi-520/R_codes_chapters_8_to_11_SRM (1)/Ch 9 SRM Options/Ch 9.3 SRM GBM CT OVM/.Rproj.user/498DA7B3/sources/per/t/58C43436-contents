# 9.3 SRM GBM CT OVM Test.R
# GBMOVM, ES and AS BOVMs
# rmarkdown::render("9.3 SRM GBM CT OVM Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  beepr - functions for beeping to let you know when program is finished
Packages <- c("beepr") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Functions defined in:
#   GBMOVM w Greeks Functions.R
#   ESGBMBINOVM With SRM Functions.R (European-style only)
#   ASGBMBINOVM With SRM Functions.R (American-style also)
#
# Test inputs
# GBMOVM inputs
inputStockPrice = 100       # Need "input" as using variable names below
inputStrikePrice = 100
inputInterestRate = 5.0        # In percent
inputDividendYield = 5.0       # In percent
inputVolatility = 30.0         # In percent
inputTimeToMaturity = 1.0
inputType = 1                  # 1 for call, -1 for put
# Additional BOVM inputs
inputEMMProbability = 50.0     # In percent
inputGreekIncrement = 1.0      # In percent of greek underlying variable
inputNumberOfSteps = as.integer(250) # Or use L: 1000L
inputPayoutType = 1L        # 1 Plain vanilla, 2 digital (digital not built)
inputStyle = 2              # 1 for terminal ES, 2 for AS
inputDigitalPayout = 100    # In dollars
# Plot ranges
LowerBound = inputStockPrice*0.5 # Stock price
UpperBound = inputStockPrice*1.5
LowerBoundV = inputVolatility*0.5 # Volatility
UpperBoundV = inputVolatility*1.5
LowerBoundT = inputTimeToMaturity*0.5 # Time to maturity
UpperBoundT = inputTimeToMaturity*1.5
LowerBoundD = 0.0 # Dividend yield
UpperBoundD = 25.0

NumberOfObservations = 51
#
#  BINInputData - list of inputs with associated names
#
BINInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
  inputDividendYield, inputVolatility, inputTimeToMaturity, inputType, 
  inputNumberOfSteps, inputPayoutType, inputStyle,
  inputEMMProbability, inputDigitalPayout, inputGreekIncrement)
names(BINInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
  "DividendYield", "Volatility", "TimeToMaturity", "Type", 
  "NumberOfSteps", "PayoutType", "Style", 
  "EMMProbability", "DigitalPayout", "GreekIncrement")
#
# GBM list
#
GBMInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
  inputDividendYield, inputVolatility, inputTimeToMaturity, inputType)
names(GBMInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
  "DividendYield", "Volatility", "TimeToMaturity", "Type")
#
# Source the appropriate functions
#
source("GBMOVM With SRM Functions.R")
source('ESGBMBINOVM With SRM Functions.R')
source('ASGBMBINOVM With SRM Functions.R')
# Console Rounding Setting
RDigits <- 4
# source("Core Functions Tests.R")
source("GBMOVM Stock Price Plots.R")
source("GBMOVM Volatility Plots.R")
source("GBMOVM Time To Maturity Plots.R")
source("GBMOVM Dividend Yield Plots.R")
beep(sound = 3, print('Finished')) # fanfare
