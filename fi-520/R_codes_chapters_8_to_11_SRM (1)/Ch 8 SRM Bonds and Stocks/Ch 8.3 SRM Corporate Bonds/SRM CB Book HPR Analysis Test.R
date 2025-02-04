# SRM CB Book HPR Analysis Test.R
# rmarkdown::render("SRM CB Book HPR Analysis Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
#  optimx - general purpose optimization
#  openxlsx - manipulate spreadsheet files
#  beepr - beeper
Packages <- c("date", "optimx", "openxlsx", "tis", "beepr") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#
# Fixed Parameters
#
# LSC horizon
inputHorizon <- 7 # In days
inputFrequency <- 2
inputPar <- 1000000.0
inputChangeInYTM <- 0.1 # Effective duration and convexity
RoughMaturity <- 4.9 # Years
NBCFactors <- 3 # Number of base curve factors including Level, 8 or less
NSCFactors <- 3 # Number of spread curve factors including Level, 8 or less
NBaseCurve <- 30 # Potential observation for every year for 30 years
# Plot range information
FixRange <- FALSE # For plots
FRMax <- 3.1 # Plot bounds if fixed
FRMin <- 2.4
# Input files for U. S. Treasury bond and CMT rates
USTFileName <- 'UST20200619.xlsx'
CBFileName <- 'CB20200619.xlsx'
CMTFileName <- 'CMT20200619.xlsx' # Should have same date as UST
mTitle = "UST: June 19, 2020" # Date in graph title
# Downloaded UST data stored with date appended: use for settlement
SettlementDateMonth = 6     # Based on file name
SettlementDateDay = 19 + 2  # Current practice is + 2 days settlement
SettlementDateYear = 2020   
source("UST Book Inputs.R") # Access UST book
source("CB Book Inputs.R") # Access corporate bond book
source("SRM CB Functions.R") # Corporate bond functions (semi-annual only)
source("SRM CB LSC Functions.R") # CB LSC functions 
source("SRM BC and SC Analysis.R")
source("SRM UST Functions.R") # UST functions (semi-annual only)
source("SRM UST LSC Functions.R") # UST LSC functions 
source("SRM LSC Factors.R")
source("SRM Basic Plots.R") # Selected basic plots
source("SRM CB Plots.R") # UST LSC functions

beep(sound = 2)






















