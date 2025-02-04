# 10.3 SRM Interest Rate Swaps Test.R
# LSC model with swap data
# rmarkdown::render("10.3 SRM Interest Rate Swaps Test.R", "word_document")
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
#  MASS - Multivariate normal random number generator
#  tis - isBusinessDay
Packages <- c("date", "optimx", "openxlsx", "stats", "expss", "dplyr", 
  "MASS", "tis") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Identify spreadsheet containing inputs
InputFileName <- 'SRM IRS Input File.xlsx'
source('SRM IRS Input Management.R')
source('SWAP GC Functions.R')
# Swap tests
SWAP = SVInputs
# 'Rate1' uses two different curves, forward curve and discount curve (2 LSCs)
SWAP$Output = 'Rate1'
SwapRateV1 <- SwapRateGC(SWAP)
SWAP$Output = 'Rate2' # Discount curve only
SwapRateV2 <- SwapRateGC(SWAP)
T1 <- TFlt1(SWAP)
T2 <- TFlt2(SWAP)
T3 <- TFix1(SWAP)
SwapRateV3 <- 100*(T1 - T2)/T3
SwapRateV1
SwapRateV2
SwapRateV3
SwapRateBC1 <- SwapRates3T(SWAP)
SwapRateBC1
# SWAP$FixedRate <- SwapRateBC1
SwapValueBC1 <- SwapValue3T(SWAP)
SwapValueBC1
SWAP$FixedRate <- SwapRateV1
SWAP$Output = 'Value1'
SwapValueV1 <- SwapRateGC(SWAP)
# SWAP$FixedRate <- SwapRateV2
SWAP$Output = 'Value2'
SwapValueV2 <- SwapRateGC(SWAP)
SwapValueV3 <- SwapValue3T(SWAP)
SwapValueV1
SwapValueV2
SwapValueV3
# Plot footer information
TN = paste0(SWAP$TradeName)
TEval = paste0(', Eval. Date: ', SWAP$EM, '/', SWAP$ED, '/', SWAP$EY)
TMat = paste0(', Mat. Date: ', SWAP$MM, '/', SWAP$MD, '/', SWAP$MY)
TNAmt = paste0(', $',formatC(as.integer(SWAP$NAmt), big.mark=',',
  format = 'f', digits = 0))
TFR = paste0(', FC: L=', FRParm1, ', S=', FRParm2, ', C=', FRParm3)
TDR = paste0(', BC: L=', GRParm1, ', S=', GRParm2, ', C=', GRParm3)
sTitle = paste0(TN, TEval, TMat, TNAmt, TFR, TDR)
sTitle
#
# Valuation functions
#
source('SRM SWAP Functions.R')
SWAP$Output = 'Rate1'
SwapRate1 <- SwapRates3T(SWAP) # SwapRateGC(SWAP)
SwapRate1
# Swap valuations
# SWAP$FixedRate <- SwapRate1
VSwapt <- SwapValuett(SWAP) # Swap value at time t
VSwaptd <- SwapValuetdtd(SWAP, HDInputs) # Swap value at horizon (td)
# Swap value at horizon (td) with parameters from initial (t)
VSwapttd <- SwapValuettd(SWAP, HDInputs) 
# Swap value at horizon (td) with forward curve parameters from horizon (td)
#  and basis curve parameters from initial (t)
VSwapFCtd <- SwapValueFCtdBCttd(SWAP, HDInputs)
# Swap value at horizon (td) with forward curve parameters from initial (t)
#  and basis curve parameters from horizon (td)
VSwapBCtd <- SwapValueFCtBCtdtd(SWAP, HDInputs)
VSwapt; VSwaptd; VSwapttd; VSwapFCtd; VSwapBCtd
#
# Analysis of first derivatives
#
source('SRM SWAP First Derivative Functions.R')
# Syntax:
#  FD - analytic first derivative; NFD - numerical first derivative (for audit)
#  FC - forward curve; BC - basis curve
#  L - level; S - slope; C1 - curvature 1
# First derivatives wrt forward curve (FC)
FDwrtFCL(SWAP, HDInputs)
NFDwrtFCL(SWAP, HDInputs)
FDwrtFCS(SWAP, HDInputs)
NFDwrtFCS(SWAP, HDInputs)
FDwrtFCC1(SWAP, HDInputs)
NFDwrtFCC1(SWAP, HDInputs)
# First derivatives wrt basis curve
FDwrtBCL(SWAP, HDInputs)
NFDwrtBCL(SWAP, HDInputs)
FDwrtBCS(SWAP, HDInputs)
NFDwrtBCS(SWAP, HDInputs)
FDwrtBCC1(SWAP, HDInputs)
NFDwrtBCC1(SWAP, HDInputs)
#
# Analysis of second derivatives
#
source('SRM SWAP Second Derivative Functions.R')
# Second derivatives wrt forward curve
SDwrtFCL(SWAP, HDInputs)
NSDwrtFCL(SWAP, HDInputs)
SDwrtFCS(SWAP, HDInputs)
NSDwrtFCS(SWAP, HDInputs)
SDwrtFCC1(SWAP, HDInputs)
NSDwrtFCC1(SWAP, HDInputs)
# Second derivatives wrt basis curve
SDwrtBCL(SWAP, HDInputs)
NSDwrtBCL(SWAP, HDInputs)
SDwrtBCS(SWAP, HDInputs)
NSDwrtBCS(SWAP, HDInputs)
SDwrtBCC1(SWAP, HDInputs)
NSDwrtBCC1(SWAP, HDInputs)
#
# Analysis of selected cross derivatives
#
source('SRM SWAP Cross Derivative Functions.R')
# Cross derivatives wrt forward curve
SDwrtFCLS(SWAP, HDInputs)
NSDwrtFCLS(SWAP, HDInputs)
SDwrtFCSC1(SWAP, HDInputs)
NSDwrtFCSC1(SWAP, HDInputs)
SDwrtFCLC1(SWAP, HDInputs)
NSDwrtFCLC1(SWAP, HDInputs)
# Cross derivatives wrt basis curve
SDwrtBCLS(SWAP, HDInputs)
NSDwrtBCLS(SWAP, HDInputs)
SDwrtBCSC1(SWAP, HDInputs)
NSDwrtBCSC1(SWAP, HDInputs)
SDwrtBCLC1(SWAP, HDInputs)
NSDwrtBCLC1(SWAP, HDInputs)
#
# Factor durations and convexities
#
source('SRM SWAP Factor Duration and Convexity Functions.R')
#
# Factor durations
#
# Forward curve
FDFCL(SWAP, HDInputs)
FDFCS(SWAP, HDInputs)
FDFCC1(SWAP, HDInputs)
# Basis curve
FDBCL(SWAP, HDInputs)
FDBCS(SWAP, HDInputs)
FDBCC1(SWAP, HDInputs)
#
# Factor convexities
#
# Forward curve
FCxFCL(SWAP, HDInputs)
FCxFCS(SWAP, HDInputs)
FCxFCC1(SWAP, HDInputs)
# Basis curve
FCxBCL(SWAP, HDInputs)
FCxBCS(SWAP, HDInputs)
FCxBCC1(SWAP, HDInputs)
#
# Factor cross-convexities
#
# Forward curve
FCCFCLS(SWAP, HDInputs)
FCCFCSC1(SWAP, HDInputs)
FCCFCLC1(SWAP, HDInputs)
# Basis curve
FCCBCLS(SWAP, HDInputs)
FCCBCSC1(SWAP, HDInputs)
FCCBCLC1(SWAP, HDInputs)


