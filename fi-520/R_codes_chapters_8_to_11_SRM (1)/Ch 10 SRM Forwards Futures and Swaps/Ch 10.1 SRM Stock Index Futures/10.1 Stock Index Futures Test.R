# 10.1 Stock Index Futures Test.R
# read.xlsx, plot, lines, legend
# Working with futures prices
# rmarkdown::render("10.1 Stock Index Futures Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
#
# Times New Roman is not available in PostScript
#
#par(family = 'Times New Roman') # Globally set fonts for graphs
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
  "Hmisc") # Libraries ("SDMTools" removed, now unsupported)
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#
# Common information for all functions
#
File = 'SPX LSC.xlsx'
mTitle1 <- 'US S&P 500'
#
# Plots (Pseudo possible, fix limits--better for analysis)
#  Produces one plot for every day
#  Open all *.jpg files and print to PDF
#  Open PDF and scroll through in full screen mode
# 
PlotPseudoPrices <- TRUE # TRUE: Plot both prices and pseudo prices
FixedLimits <- TRUE # Used for daily plots
# source('Period Plots.R', print.eval = TRUE)
#
# unlink('*.jpg') # Rather dangerous, deletes all files with extension jpg
#
# Selected Term Premium Analysis
#
NumberOfNearbys = 3
Title1 = as.character('S&P 500')
FPFixedBounds = TRUE # Fix y-axis for Marginal Term Premiums
FPUpperBound = 2100
FPLowerBound = 600
TPFixedBounds = TRUE # Fix y-axis for Marginal Term Premiums
TPUpperBound = 1.5
TPLowerBound = -1.5
# FP v Date, TP1, TP3 and Rates v Date
StartDate = 20041231
EndDate = 20091231
sTitle = expression(paste("12/31/2004 through 12/31/2009"))
source('Futures Term Premiums.R')
StartDate = 20091231
EndDate = 20141231
sTitle = expression(paste("12/31/2009 through 12/31/2014"))
source('Futures Term Premiums.R')
# MTP, Rates v Date (pick nearbys to plot)
Plots <- c(3) # Identifies which maturity plots to generate (1-12, up to 12)
IncludeRates <- TRUE # Include period rates in term premium graphs
StartDate = 20041231
EndDate = 20091231
sTitle = expression(paste("12/31/2004 through 12/31/2009"))
source('Term Premium Plots.R', print.eval = TRUE)
StartDate = 20091231
EndDate = 20141231
sTitle = expression(paste("12/31/2009 through 12/31/2014"))
source('Term Premium Plots.R', print.eval = TRUE)
#
# Percentage Change in Futures
#
PCFFixedBounds = TRUE # Fix y-axis for Percentage Change in Futures
PCFUpperBound = 2.5
PCFLowerBound = -2.5
StartDate = 20041231
EndDate = 20091231
sTitle = expression(paste("12/31/2004 through 12/31/2009"))
source('Percentage Change in Futures Plots.R', print.eval = TRUE)
StartDate = 20091231
EndDate = 20141231
sTitle = expression(paste("12/31/2009 through 12/31/2014"))
source('Percentage Change in Futures Plots.R', print.eval = TRUE)
#
# Marginal Contribution Analysis
#
MCFixedBounds <- TRUE
MCUpperBound <- TPUpperBound
MCLowerBound <- TPLowerBound
StartDate = 20041231
EndDate = 20091231
sTitle = expression(paste("12/31/2004 through 12/31/2009"))
source('Marginal Contribution Plots.R')
StartDate = 20091231
EndDate = 20141231
sTitle = expression(paste("12/31/2009 through 12/31/2014"))
source('Marginal Contribution Plots.R')
# #
# # Selected Measures Evidencing Carry Arbitrage
# #
RollingWindow <- 26 # For exponentially weighted moving average
Lambda = 0.94 # Base case: 0.94 (0.9999 is equally weighted)
# LengthArb <- 100 # Maximum number of contracts to store arb data
ArbMaturity = 6 # Coded for 6 months or 12 months
CAAFixedBounds = FALSE # Fix y-axis for Marginal Contributions
CAAUpperBound = 1.0
CAALowerBound = -1.0
StartDate = 20041231
EndDate = 20091231
sTitle = expression(paste("12/31/2004 through 12/31/2009"))
source('CAA Process Generic Data.R')
StartDate = 20091231
EndDate = 20141231
sTitle = expression(paste("12/31/2009 through 12/31/2014"))
source('CAA Process Generic Data.R')
