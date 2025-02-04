# Global Outbreaks BATCH.R
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
#  optimx - general purpose optimization
#  openxlsx - manipulate spreadsheet files
#  stats - general statistical functions
#  expss - vlookup (not used now)
#  dplyr - rounding in dataframes
Packages <- c("date", "optimx", "openxlsx", "stats", "expss", "dplyr") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Identify spreadsheet containing inputs
InputFileName <- 'Global Outbreaks Input File.xlsx'
#
# Core modeling inputs
#
NumberOfMaturities <- 500
#
# Compute SRM and DRM of base model
#
source("LSC Model Calibration.R")
source("LSC Model Calibration Plots.R")
source("LSC Model Calibration Plots V2.R")
#
# Create multiple spreadsheets of different scenarios
#

#
# Static risk measures
#
source("Traditional Model Greeks.R")
source("LSC Model Greeks.R")
source("LSC Model Bar Charts.R")
# 
# Begin work on valuation impacts
#  Paper Figure 1 Replication
#
source("Replication of Figure 1.R")
#
# Paper Figure 2 Replication (not possible due to errors)
#  Traditional Model (New method for handling growth and WACC)
#  Extended to LSC Model (Modified slope)
#
source("Replication of Figure 2 Both Methods.R")



