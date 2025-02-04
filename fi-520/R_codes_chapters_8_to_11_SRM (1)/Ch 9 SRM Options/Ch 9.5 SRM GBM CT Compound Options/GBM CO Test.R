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
# List expected as input in SpreadOptionValue
#
inputiC <- 1                              # 1-CO call, -1-CO put
inputiU <- 1                              # 1-UO call, -1-UO put
inputUnderlying <- 100.0                  # Must be positive
inputUnderlyingStrikePrice <- 50.0      # Must be positive
inputCompoundStrikePrice <- 50.0          # Must be positive
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
# source("BSMOVM and Extended Functions.R")
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
UCallValue <- BSMOptionValue(BSMInputData)
BSMInputData$Type <- -1
UPutValue <- BSMOptionValue(BSMInputData)
UCallValue; UPutValue
# Base case
COInputData$iC <- 1
COInputData$iU <- 1
CoCCSP <- COCriticalStockPrice(COInputData, UCallValue, LowerBound, UpperBound)
CoC <- COValue(COInputData, LowerBound, UpperBound)
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
BSMInputData$Type <- 1
BSMInputData$StrikePrice <- COInputData$XU + COInputData$XC
C <- BSMOptionValue(BSMInputData)
d1a <- d1(BSMInputData)
d2a <- d2(BSMInputData)
Nd1 <- NAp(d1a)
Nd2 <- NAp(d2a)
d11; d12; d21; d22
d1a; d2a
N2d11d12; N2d21d22; N1d21
Nd1; Nd2
CoC; C
Part1 <- COInputData$S*N2d11d12
Part2 <- -COInputData$XU*exp(-(COInputData$r/100.0)*COInputData$TU)*N2d21d22
Part3 <- -COInputData$XC*exp(-(COInputData$r/100.0)*COInputData$TC)*N1d21
SumCoC <- Part1 + Part2 + Part3
Part1; Part2; Part3; SumCoC


CoCNGDelta <- CONGDelta(COInputData, LowerBound, UpperBound)
CoCNGGamma <- CONGGamma(COInputData, LowerBound, UpperBound)
CoCNGTheta <- CONGTheta(COInputData, LowerBound, UpperBound)
COInputData$iU <- -1
CoPCSP <- COCriticalStockPrice(COInputData, UPutValue, LowerBound, UpperBound)
CoP <- COValue(COInputData, LowerBound, UpperBound)
CoPNGDelta <- CONGDelta(COInputData, LowerBound, UpperBound)
CoPNGGamma <- CONGGamma(COInputData, LowerBound, UpperBound)
CoPNGTheta <- CONGTheta(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- 1
PoCCSP <- COCriticalStockPrice(COInputData, UCallValue, LowerBound, UpperBound)
PoC <- COValue(COInputData, LowerBound, UpperBound)
PoCNGDelta <- CONGDelta(COInputData, LowerBound, UpperBound)
PoCNGGamma <- CONGGamma(COInputData, LowerBound, UpperBound)
PoCNGTheta <- CONGTheta(COInputData, LowerBound, UpperBound)
COInputData$iU <- -1
PoPCSP <- COCriticalStockPrice(COInputData, UPutValue, LowerBound, UpperBound)
PoP <- COValue(COInputData, LowerBound, UpperBound)
PoPNGDelta <- CONGDelta(COInputData, LowerBound, UpperBound)
PoPNGGamma <- CONGGamma(COInputData, LowerBound, UpperBound)
PoPNGTheta <- CONGTheta(COInputData, LowerBound, UpperBound)
CoCCSP; CoPCSP; PoCCSP; PoPCSP
CoC; CoP; PoC; PoP
CoCNGDelta; CoPNGDelta; PoCNGDelta; PoPNGDelta
CoCNGGamma; CoPNGGamma; PoCNGGamma; PoPNGGamma


# Lower Bounds
COInputData$iC <- 1
COInputData$iU <- 1
CoCLB <- COLowerBound(COInputData)
COInputData$iC <- 1
COInputData$iU <- -1
CoPLB <- COLowerBound(COInputData)
COInputData$iC <- -1
COInputData$iU <- 1
PoCLB <- COLowerBound(COInputData)
COInputData$iC <- -1
COInputData$iU <- -1
PoPLB <- COLowerBound(COInputData)
CoCLB; CoPLB; PoCLB; PoPLB
# Upper Bounds
COInputData$iC <- 1
COInputData$iU <- 1
CoCUB <- COUpperBound(COInputData)
COInputData$iC <- 1
COInputData$iU <- -1
CoPUB <- COUpperBound(COInputData)
COInputData$iC <- -1
COInputData$iU <- 1
PoCUB <- COUpperBound(COInputData)
COInputData$iC <- -1
COInputData$iU <- -1
PoPUB <- COUpperBound(COInputData)
CoCUB; CoPUB; PoCUB; PoPUB
# Delta
COInputData$iC <- 1
COInputData$iU <- 1
CoCDelta <- CODelta(COInputData, LowerBound, UpperBound)
COInputData$iC <- 1
COInputData$iU <- -1
CoPDelta <- CODelta(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- 1
PoCDelta <- CODelta(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- -1
PoPDelta <- CODelta(COInputData, LowerBound, UpperBound)
# Gamma
COInputData$iC <- 1
COInputData$iU <- 1
CoCGamma <- COGamma(COInputData, LowerBound, UpperBound)
COInputData$iC <- 1
COInputData$iU <- -1
CoPGamma <- COGamma(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- 1
PoCGamma <- COGamma(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- -1
PoPGamma <- COGamma(COInputData, LowerBound, UpperBound)
# Theta
COInputData$iC <- 1
COInputData$iU <- 1
CoCTheta <- COTheta(COInputData, LowerBound, UpperBound)
COInputData$iC <- 1
COInputData$iU <- -1
CoPTheta <- COTheta(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- 1
PoCTheta <- COTheta(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- -1
PoPTheta <- COTheta(COInputData, LowerBound, UpperBound)

CoC; CoP; PoC; PoP
CoCLB; CoPLB; PoCLB; PoPLB
CoCUB; CoPUB; PoCUB; PoPUB
CoCCSP; CoPCSP; PoCCSP; PoPCSP
CoCDelta; CoPDelta; PoCDelta; PoPDelta
CoCNGDelta; CoPNGDelta; PoCNGDelta; PoPNGDelta
CoCGamma; CoPGamma; PoCGamma; PoPGamma
CoCNGGamma; CoPNGGamma; PoCNGGamma; PoPNGGamma

xCoC <- (COInputData$r/100)*CoC -
  (COInputData$r/100 - COInputData$q/100)*CoCDelta*COInputData$S -
  0.5*(COInputData$v/100)^2*COInputData$S*CoCGamma
xCoP <- (COInputData$r/100)*CoP -
  (COInputData$r/100 - COInputData$q/100)*CoPDelta*COInputData$S -
  0.5*(COInputData$v/100)^2*COInputData$S*CoPGamma
xPoC <- (COInputData$r/100)*PoC -
  (COInputData$r/100 - COInputData$q/100)*PoCDelta*COInputData$S -
  0.5*(COInputData$v/100)^2*COInputData$S*PoCGamma
xPoP <- (COInputData$r/100)*PoP -
  (COInputData$r/100 - COInputData$q/100)*PoPDelta*COInputData$S -
  0.5*(COInputData$v/100)^2*COInputData$S*PoPGamma

CoCTheta; CoPTheta; PoCTheta; PoPTheta
CoCNGTheta; CoPNGTheta; PoCNGTheta; PoPNGTheta
xCoC; xCoP; xPoC; xPoP
CoC; CoP; PoC; PoP

# Vega
COInputData$iC <- 1
COInputData$iU <- 1
CoCVega <- COVega(COInputData, LowerBound, UpperBound)
CoCNGVega <- CONGVega(COInputData, LowerBound, UpperBound)
COInputData$iC <- 1
COInputData$iU <- -1
CoPVega <- COVega(COInputData, LowerBound, UpperBound)
CoPNGVega <- CONGVega(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- 1
PoCVega <- COVega(COInputData, LowerBound, UpperBound)
PoCNGVega <- CONGVega(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- -1
PoPVega <- COVega(COInputData, LowerBound, UpperBound)
PoPNGVega <- CONGVega(COInputData, LowerBound, UpperBound)

CoCVega; CoPVega; PoCVega; PoPVega
CoCNGVega; CoPNGVega; PoCNGVega; PoPNGVega

# Rho
COInputData$iC <- 1
COInputData$iU <- 1
CoCRho <- CORho(COInputData, LowerBound, UpperBound)
CoCNGRho <- CONGRho(COInputData, LowerBound, UpperBound)
COInputData$iC <- 1
COInputData$iU <- -1
CoPRho <- CORho(COInputData, LowerBound, UpperBound)
CoPNGRho <- CONGRho(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- 1
PoCRho <- CORho(COInputData, LowerBound, UpperBound)
PoCNGRho <- CONGRho(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- -1
PoPRho <- CORho(COInputData, LowerBound, UpperBound)
PoPNGRho <- CONGRho(COInputData, LowerBound, UpperBound)

CoCRho; CoPRho; PoCRho; PoPRho
CoCNGRho; CoPNGRho; PoCNGRho; PoPNGRho


