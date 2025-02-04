# 10.2 Incremental Carry Costs Test.R
# read.xlsx, plot, lines, legend
# Working with futures prices
# rmarkdown::render("10.1 Stock Index Futures Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Technical plot manipulations require resetting back to default
defaultpar <- par() # plot global parameters
# Libraries
#  openxlsx: MS Excel file management
#  zoo: time-series management
#  date: calendar time
#  moments: Skewness and kurtosis
#  stats: Statistical functions
#  PerformanceAnalytics: Higher moments
#  RColorBrewer: 
#  SDMTools: 
#  weights:
#  xts: Extended time series
#  rootSolve: 
#  Weighted.Desc.Stat: 
Packages <- c("openxlsx", "zoo", "date", "moments", "stats",
  "PerformanceAnalytics", "RColorBrewer", "weights",
  "xts", "rootSolve", "Weighted.Desc.Stat",
  "Hmisc") # Libraries
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#
# Common information for all functions
#
StartDate = 20141231
EndDate = 20191231
sTitle = expression(paste("12/31/2014 through 12/31/2019"))
Plots <- c(3) # Identifies which maturity plots to generate (1-12, up to 12)
IncludeRates <- TRUE # Include period rates in term premium graphs
IMCCFixedBounds = TRUE # Fix y-axis for Implied Marginal Carry Costs
IMCCUpperBound = 16.0
IMCCLowerBound = -16.0
#
# U. S. Metals Market
#
File = 'Gold2 LSC.xlsx'
mTitle1 <- 'US Gold'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'Silver LSC.xlsx'
mTitle1 <- 'US Silver'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'Copper LSC.xlsx'
mTitle1 <- 'US Copper'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
#
# China Metals Market
#
File = 'CNYGold LSC.xlsx'
mTitle1 <- 'China Gold'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'CNYSilver LSC.xlsx'
mTitle1 <- 'China Silver'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'CNYCopper LSC.xlsx'
mTitle1 <- 'China Copper'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
#
# U. S. Energy Market
#
IMCCFixedBounds = TRUE # Fix y-axis for Implied Marginal Carry Costs
IMCCUpperBound = 50.0
IMCCLowerBound = -50.0
File = 'Brent LSC.xlsx'
mTitle1 <- 'US Brent Crude Oil'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'WTIComb LSC.xlsx'
mTitle1 <- 'US WTI Crude Oil'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'GasOil LSC.xlsx'
mTitle1 <- 'US Diesel'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'UnleadedGasComb LSC.xlsx'
mTitle1 <- 'US Unleaded Gas'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'NGComb LSC.xlsx'
mTitle1 <- 'US Natural Gas'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'HeatingOilComb LSC.xlsx'
mTitle1 <- 'US Heating Oil'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
#
# U. S. Soybean Market
#
IMCCFixedBounds = TRUE # Fix y-axis for Implied Marginal Carry Costs
IMCCUpperBound = 50.0
IMCCLowerBound = -50.0
File = 'Soybeans LSC.xlsx'
mTitle1 <- 'US Soybeans'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'SoybeanMeal LSC.xlsx'
mTitle1 <- 'US Soybean Meal'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'SoybeanOil LSC.xlsx'
mTitle1 <- 'US Soybean Oil'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
#
# China Soybean Market
#
File = 'CNYSoybeanNo1 LSC.xlsx'
mTitle1 <- 'China Soybean No. 1'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'CNYSoybeanNo2 LSC.xlsx'
mTitle1 <- 'China Soybean No. 2'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'CNYSoybeanMeal LSC.xlsx'
mTitle1 <- 'China Soybean Meal'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
File = 'CNYSoybeanOil LSC.xlsx'
mTitle1 <- 'China Soybean Oil'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
#
# U. S. Wheat Market
#
IMCCFixedBounds = TRUE # Fix y-axis for Implied Marginal Carry Costs
IMCCUpperBound = 50.0
IMCCLowerBound = -50.0
File = 'Wheat LSC.xlsx'
mTitle1 <- 'US Wheat'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)
#
# China Wheat Market
#
File = 'CNYSGWheat LSC.xlsx'
mTitle1 <- 'China Strong Gluten Wheat'
source('Implied Marginal Carry Costs Plots.R', print.eval = TRUE)

