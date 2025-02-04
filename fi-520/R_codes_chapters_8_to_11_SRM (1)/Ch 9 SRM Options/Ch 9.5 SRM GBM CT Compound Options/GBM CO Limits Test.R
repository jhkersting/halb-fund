# GBM CO Greeks.R
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
# Illustration of convergence to BSMOVM 
#
# Case 1: TU = TC => X = XU + XC (for call); X = XC (for put, XU = 0 -- PCP)
#
inputiC <- 1                              # 1-CO call, -1-CO put
inputiU <- 1                              # 1-UO call, -1-UO put
inputUnderlying <- 100.0                  # Must be positive
inputUnderlyingStrikePrice <- 0.00001      # Must be positive
inputCompoundStrikePrice <- 100.0          # Must be positive
inputInterestRate <- 5.0 #log(1.1)*100                  # Must be positive
inputUnderlyingYield <- 0.0              # Payout of underlying instrument
inputOptionYield <- 0.0 #log(1.05)*100                    # Payout of option
inputVolatility <- 30.0                   # Must be positive
inputUnderlyingTimeToMaturity <- 1.00001     # Must be positive
inputCompoundTimeToMaturity <- 1.0        # Must be positive
inputGreekIncrement = 0.001   # In percent of greek underlying variable
COInputData <- list(inputiC, inputiU, inputUnderlying, 
  inputUnderlyingStrikePrice, inputCompoundStrikePrice, inputInterestRate,
  inputUnderlyingYield, inputOptionYield, inputVolatility, 
  inputUnderlyingTimeToMaturity, inputCompoundTimeToMaturity,
  inputGreekIncrement) 
names(COInputData) <- c("iC", "iU", "S", "XU", "XC", "r", "d", "q", "v", 
  "TU", "TC", "Increment")
source("GBM COVM w Greeks Functions.R")
# Boundaries for critical stock price iterative search
LowerBound <- 0
UpperBound <- 5.0*COInputData$S #Critical stock price for put has 2 solutions
# Tests
inputUnderlying <- COInputData$S
inputUnderlyingStrikePrice <- COInputData$XU
inputInterestRate <- COInputData$r
inputUnderlyingYield <- COInputData$d
inputOptionYield <- COInputData$q
inputVolatility <- COInputData$v
inputTimeToMaturity <- COInputData$TU
inputType <- COInputData$iU
BSMInputData <- list(inputUnderlying, inputUnderlyingStrikePrice, 
  inputInterestRate, inputUnderlyingYield,  inputOptionYield, 
  inputVolatility, inputTimeToMaturity, inputType)
names(BSMInputData) <- c("StockPrice", "StrikePrice", "InterestRate",
  "DividendYield", "OptionYield", "Volatility", "TimeToMaturity", "Type")
# Base case--Call on call
COInputData$iC <- 1
COInputData$iU <- 1
CoC <- COValue(COInputData, LowerBound, UpperBound)
BSMInputData$Type <- 1
BSMInputData$StrikePrice <- COInputData$XU + COInputData$XC
C <- BSMOptionValue(BSMInputData)
CoC; C
# Supplemental data
CoCCSP <- COCriticalStockPrice(COInputData, C, LowerBound, UpperBound)
d11 <- COd11(COInputData, LowerBound, UpperBound)
d12 <- COd12(COInputData)
d21 <- COd21(COInputData, LowerBound, UpperBound)
d22 <- COd22(COInputData)
mean1 <- rep(0,2)
lower1 <- rep(-Inf,2)
corr1 <- diag(2)
corr1[lower.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
corr1[upper.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
upper1 <- c(COInputData$iC*COInputData$iU*d11, COInputData$iU*d12)
N2d11d12 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]

upper1 <- c(COInputData$iU*d12, COInputData$iC*COInputData$iU*d11)
N2d12d11 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]

COInputData$iC*COInputData$iU*d11
COInputData$iU*d12
N2d11d12; N2d12d11



upper1 <- c(COInputData$iC*COInputData$iU*d21, COInputData$iU*d22)
N2d21d22 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
N1d21 <- NAp(d21)
d1a <- d1(BSMInputData)
d2a <- d2(BSMInputData)
Nd1 <- NAp(BSMInputData$Type*d1a)
Nd2 <- NAp(BSMInputData$Type*d2a)
d11; d12; d21; d22
d1a; d2a
N2d11d12; N2d21d22; N1d21
Nd1; Nd2
CoC; C
Part1 <- COInputData$iC*COInputData$iU*COInputData$S*N2d11d12
Part2 <- -COInputData$iC*COInputData$iU*COInputData$XU*
  exp(-(COInputData$r/100.0)*COInputData$TU)*N2d21d22
Part3 <- -COInputData$iC*COInputData$iU*COInputData$XC*
  exp(-(COInputData$r/100.0)*COInputData$TC)*N1d21
SumCoC <- Part1 + Part2 + Part3
Part1; Part2; Part3; SumCoC
#
# Put on call
#
# Base case--Put on call
COInputData$XC = COInputData$XC + COInputData$XU
COInputData$XU = 0.000001
COInputData$iC <- -1
COInputData$iU <- 1
PoC <- COValue(COInputData, LowerBound, UpperBound)
BSMInputData$Type <- -1
BSMInputData$StrikePrice <- COInputData$XU + COInputData$XC
P <- BSMOptionValue(BSMInputData)
PoC; P
# Supplemental data
PoCCSP <- COCriticalStockPrice(COInputData, P, LowerBound, UpperBound)
d11 <- COd11(COInputData, LowerBound, UpperBound)
d12 <- COd12(COInputData)
d21 <- COd21(COInputData, LowerBound, UpperBound)
d22 <- COd22(COInputData)
mean1 <- rep(0,2)
lower1 <- rep(-Inf,2)
corr1 <- diag(2)
corr1[lower.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
corr1[upper.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
upper1 <- c(COInputData$iC*COInputData$iU*d11, COInputData$iU*d12)
N2d11d12 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
upper1 <- c(COInputData$iC*COInputData$iU*d21, COInputData$iU*d22)
N2d21d22 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
N1d21 <- NAp(COInputData$iC*COInputData$iU*d21)
d1a <- d1(BSMInputData)
d2a <- d2(BSMInputData)
Nd1 <- NAp(BSMInputData$Type*d1a)
Nd2 <- NAp(BSMInputData$Type*d2a)
d11; d12; d21; d22
d1a; d2a
N2d11d12; N2d21d22; N1d21
Nd1; Nd2
PoC; P
Part1 <- COInputData$iC*COInputData$iU*COInputData$S*N2d11d12
Part2 <- -COInputData$iC*COInputData$iU*COInputData$XU*
  exp(-(COInputData$r/100.0)*COInputData$TU)*N2d21d22
Part3 <- -COInputData$iC*COInputData$iU*COInputData$XC*
  exp(-(COInputData$r/100.0)*COInputData$TC)*N1d21
SumPoC <- Part1 + Part2 + Part3
Part1; Part2; Part3; SumPoC
SumPoC; PoC
#
# XU = 0
#
# Base case--Call on call
COInputData$XU <- 0.00001
COInputData$TU <- 5.0
COInputData$TC <- 1.0
COInputData$iC <- 1
COInputData$iU <- 1
CoC <- COValue(COInputData, LowerBound, UpperBound)
BSMInputData$Type <- 1
BSMInputData$StrikePrice <- COInputData$XU + COInputData$XC
C <- BSMOptionValue(BSMInputData)
CoC; C
# Supplemental data
CoCCSP <- COCriticalStockPrice(COInputData, C, LowerBound, UpperBound)
d11 <- COd11(COInputData, LowerBound, UpperBound)
d12 <- COd12(COInputData)
d21 <- COd21(COInputData, LowerBound, UpperBound)
d22 <- COd22(COInputData)
mean1 <- rep(0,2)
lower1 <- rep(-Inf,2)
corr1 <- diag(2)
corr1[lower.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
corr1[upper.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
upper1 <- c(COInputData$iC*COInputData$iU*d11, COInputData$iU*d12)
N2d11d12 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
upper1 <- c(COInputData$iC*COInputData$iU*d21, COInputData$iU*d22)
N2d21d22 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
N1d21 <- NAp(d21)
d1a <- d1(BSMInputData)
d2a <- d2(BSMInputData)
Nd1 <- NAp(BSMInputData$Type*d1a)
Nd2 <- NAp(BSMInputData$Type*d2a)
d11; d12; d21; d22
d1a; d2a
N2d11d12; N2d21d22; N1d21
Nd1; Nd2
CoC; C
Part1 <- COInputData$iC*COInputData$iU*COInputData$S*N2d11d12
Part2 <- -COInputData$iC*COInputData$iU*COInputData$XU*
  exp(-(COInputData$r/100.0)*COInputData$TU)*N2d21d22
Part3 <- -COInputData$iC*COInputData$iU*COInputData$XC*
  exp(-(COInputData$r/100.0)*COInputData$TC)*N1d21
SumCoC <- Part1 + Part2 + Part3
Part1; Part2; Part3; SumCoC
#
# Put on call
#
# Base case--Put on call
COInputData$XC = COInputData$XC + COInputData$XU
COInputData$iC <- -1
COInputData$iU <- 1
PoC <- COValue(COInputData, LowerBound, UpperBound)
BSMInputData$Type <- -1
BSMInputData$StrikePrice <- COInputData$XU + COInputData$XC
P <- BSMOptionValue(BSMInputData)
PoC; P
# Supplemental data
PoCCSP <- COCriticalStockPrice(COInputData, P, LowerBound, UpperBound)
d11 <- COd11(COInputData, LowerBound, UpperBound)
d12 <- COd12(COInputData)
d21 <- COd21(COInputData, LowerBound, UpperBound)
d22 <- COd22(COInputData)
mean1 <- rep(0,2)
lower1 <- rep(-Inf,2)
corr1 <- diag(2)
corr1[lower.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
corr1[upper.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
upper1 <- c(COInputData$iC*COInputData$iU*d11, COInputData$iU*d12)
N2d11d12 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
upper1 <- c(COInputData$iC*COInputData$iU*d21, COInputData$iU*d22)
N2d21d22 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
N1d21 <- NAp(COInputData$iC*COInputData$iU*d21)
d1a <- d1(BSMInputData)
d2a <- d2(BSMInputData)
Nd1 <- NAp(BSMInputData$Type*d1a)
Nd2 <- NAp(BSMInputData$Type*d2a)
d11; d12; d21; d22
d1a; d2a
N2d11d12; N2d21d22; N1d21
Nd1; Nd2
PoC; P
Part1 <- COInputData$iC*COInputData$iU*COInputData$S*N2d11d12
Part2 <- -COInputData$iC*COInputData$iU*COInputData$XU*
  exp(-(COInputData$r/100.0)*COInputData$TU)*N2d21d22
Part3 <- -COInputData$iC*COInputData$iU*COInputData$XC*
  exp(-(COInputData$r/100.0)*COInputData$TC)*N1d21
SumPoC <- Part1 + Part2 + Part3
Part1; Part2; Part3; SumPoC
SumPoC; PoC


#
# TU = +inf
#
# Base case--Call on call
COInputData$XU <- 100
COInputData$XC <- 100
COInputData$TU <- 10000.0
COInputData$TC <- 1.0
COInputData$iC <- 1
COInputData$iU <- 1
CoC <- COValue(COInputData, LowerBound, UpperBound)
BSMInputData$Type <- 1
BSMInputData$StrikePrice <- COInputData$XU
C <- BSMOptionValue(BSMInputData)
CoC; C
# Supplemental data
CoCCSP <- COCriticalStockPrice(COInputData, C, LowerBound, UpperBound)
d11 <- COd11(COInputData, LowerBound, UpperBound)
d12 <- COd12(COInputData)
d21 <- COd21(COInputData, LowerBound, UpperBound)
d22 <- COd22(COInputData)
mean1 <- rep(0,2)
lower1 <- rep(-Inf,2)
corr1 <- diag(2)
corr1[lower.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
corr1[upper.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
upper1 <- c(COInputData$iC*COInputData$iU*d11, COInputData$iU*d12)
N2d11d12 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
upper1 <- c(COInputData$iC*COInputData$iU*d21, COInputData$iU*d22)
N2d21d22 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
N1d21 <- NAp(d21)
d1a <- d1(BSMInputData)
d2a <- d2(BSMInputData)
Nd1 <- NAp(BSMInputData$Type*d1a)
Nd2 <- NAp(BSMInputData$Type*d2a)
d11; d12; d21; d22
d1a; d2a
N2d11d12; N2d21d22; N1d21
Nd1; Nd2
CoC; C
Part1 <- COInputData$iC*COInputData$iU*COInputData$S*N2d11d12
Part2 <- -COInputData$iC*COInputData$iU*COInputData$XU*
  exp(-(COInputData$r/100.0)*COInputData$TU)*N2d21d22
Part3 <- -COInputData$iC*COInputData$iU*COInputData$XC*
  exp(-(COInputData$r/100.0)*COInputData$TC)*N1d21
SumCoC <- Part1 + Part2 + Part3
Part1; Part2; Part3; SumCoC
#
# Put on call
#
# Base case--Put on call
# COInputData$XC = COInputData$XC + COInputData$XU
COInputData$iC <- -1
COInputData$iU <- 1
PoC <- COValue(COInputData, LowerBound, UpperBound)
BSMInputData$Type <- -1
BSMInputData$StrikePrice <- COInputData$XU
P <- BSMOptionValue(BSMInputData)
PoC; P
# Supplemental data
PoCCSP <- COCriticalStockPrice(COInputData, P, LowerBound, UpperBound)
d11 <- COd11(COInputData, LowerBound, UpperBound)
d12 <- COd12(COInputData)
d21 <- COd21(COInputData, LowerBound, UpperBound)
d22 <- COd22(COInputData)
mean1 <- rep(0,2)
lower1 <- rep(-Inf,2)
corr1 <- diag(2)
corr1[lower.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
corr1[upper.tri(corr1)] <- COInputData$iC*sqrt(COInputData$TC/COInputData$TU)
upper1 <- c(COInputData$iC*COInputData$iU*d11, COInputData$iU*d12)
N2d11d12 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
upper1 <- c(COInputData$iC*COInputData$iU*d21, COInputData$iU*d22)
N2d21d22 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
N1d21 <- NAp(COInputData$iC*COInputData$iU*d21)
d1a <- d1(BSMInputData)
d2a <- d2(BSMInputData)
Nd1 <- NAp(BSMInputData$Type*d1a)
Nd2 <- NAp(BSMInputData$Type*d2a)
d11; d12; d21; d22
d1a; d2a
N2d11d12; N2d21d22; N1d21
Nd1; Nd2
PoC; P
Part1 <- COInputData$iC*COInputData$iU*COInputData$S*N2d11d12
Part2 <- -COInputData$iC*COInputData$iU*COInputData$XU*
  exp(-(COInputData$r/100.0)*COInputData$TU)*N2d21d22
Part3 <- -COInputData$iC*COInputData$iU*COInputData$XC*
  exp(-(COInputData$r/100.0)*COInputData$TC)*N1d21
SumPoC <- Part1 + Part2 + Part3
Part1; Part2; Part3; SumPoC
SumPoC; PoC


