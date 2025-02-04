# SRM BC and SC Analysis.R
# Select one bond for analysis
LengthUST <- length(UST$JMaturityDate) # Number of observations
z <- FALSE # Indicator switch
JTodaysDate <- as.integer(mdy.date(SettlementDateMonth, SettlementDateDay, 
                                   SettlementDateYear))
for(i in 1:LengthUST){ # Find first bond in excess of RoughMaturity
  TDate <- as.integer(UST$JMaturityDate[i])
  TYears <- (as.numeric(TDate - JTodaysDate))/365.25
  if(z == FALSE && TYears > RoughMaturity){
    SelectedBond <- i
    z <- TRUE
  }
}
UST[SelectedBond,] # Console: Selected bond parameters
inputCouponRate <- UST$COUPON[SelectedBond]
inputYieldToMaturity <- UST$ASKED.YIELD[SelectedBond]
inputBondPrice <- (UST$APrice[SelectedBond]/100.0)*inputPar
MaturityDateMonth <- month(UST$JMaturityDate[SelectedBond])
MaturityDateDay <- day(UST$JMaturityDate[SelectedBond]) 
MaturityDateYear <- year(UST$JMaturityDate[SelectedBond])
BONDInputData <- list(inputFrequency, inputCouponRate, inputPar, 
  inputYieldToMaturity, inputBondPrice, 
  SettlementDateMonth, SettlementDateDay, SettlementDateYear, 
  MaturityDateMonth, MaturityDateDay, MaturityDateYear,
  inputChangeInYTM)
names(BONDInputData) <- c("Frequency", "CouponRate", "Par", 
  "YieldToMaturity", "BondPrice", 
  "SettlementDateMonth", "SettlementDateDay", "SettlementDateYear", 
  "MaturityDateMonth", "MaturityDateDay", "MaturityDateYear",
  "ChangeInYTM")
# Calendar manipulations
N = CouponsRemaining(BONDInputData)
# ElapsedOutput contains fraction, JLastDate, JNextDate, and JCurrentDate
ElapsedOutput = Elapsed(BONDInputData)
# Number of Total Days
NTD <- ElapsedOutput$NextDate - ElapsedOutput$LastDate
# Number of Accrued Days since last semi-annual coupon
NAD <- ElapsedOutput$Fraction * NTD
# Fraction of coupon period that has elapsed already
f <- ElapsedOutput$Fraction
# Bond maturity, in years
Mat <- TimeToMaturity(BONDInputData)
NAD; NTD; f; N; Mat
# Bond value given yield to maturity
MarketQuotedBondPrice <- inputBondPrice
MarketValueOfBond <- BondValue(BONDInputData)
AccruedInterestAmount <- AccruedInterest(BONDInputData)
ModelQuotedBondPrice <- MarketValueOfBond - AccruedInterestAmount
MarketValueOfBond; AccruedInterestAmount; 
ModelQuotedBondPrice; MarketQuotedBondPrice
# Yield to maturity given bond value
inputBondPrice = MarketQuotedBondPrice #Dollars:Quoted price w/o accrued interest
BONDInputData$BondPrice <- inputBondPrice
EstYieldToMaturity = YieldToMaturitySolver(BONDInputData)
EstYieldToMaturity; inputYieldToMaturity
#
# Build zero coupon, annualized, cont. compounded, discount rate curve
#  Inputs: NBCFactors - Number of base curve factors
#  NBaseCurve - Number of CMTs. MarketCMTRates=int. vector 1 to NBaseCurve
# CMT curve information
#
source("CMT Inputs.R")
# Access CB book
source("Bond Inputs.R")
# CB functions (semi-annual only)
source("CB Functions.R")
#
# Work on base curve
#
# BCTau[1] <- TimeToMaturity(BONDInputData) # Set first BCTau to be maturity
# x filled with initial guesses
x <- numeric(NBCFactors)   # b (Base curve level, slope, and curvatures)
if(NBCTau < 2)BCSc <- numeric(1)
if(NBCTau >= 2)BCSc <- numeric(NBCTau) # Base curve scalars
for(i in 1:NBCFactors){
  if(i==1){
    x[1] <- MarketCMTRates[30]    # Level: Might be NA
    if(is.na(x[1]))x[1] <- 5.0     # Thus, set to 5%
    BCSc[1] <- 0.0
  }
  if(i==2){
    x[2] <- MarketCMTRates[1] - MarketCMTRates[30] # Slope
    if(is.na(x[2]))x[2] <- 0.0     # Defaults to zero
    BCSc[1] <- BCTau[1]
  }
  if(i>2){
    x[i] <- 0
    BCSc[i-2] <- BCTau[i-2]
  }
}
# Just quickly check input parameters for DiffCMTRates
x # Initial guess
NBCFactors
BCSc
NBaseCurve
MarketCMTRates
# Given coefficients for discount curve based on LSC, 
#  estimate sum squared difference
Answer <- DiffCMTRates(x, NBCFactors, BCSc, NBaseCurve, MarketCMTRates)
Answer
# optimx R package provides minimization routine to select LSC coefficients 
# to minimize squared differences #, all.methods=TRUE (uses all methods)
OptOutput <- optimx(par=x, fn=DiffCMTRates, NFac = NBCFactors, S = BCSc, 
  NCMTs = NBaseCurve, MSR = MarketCMTRates, 
  method=c('nlminb'), control=list(save.failures=FALSE, maxit=2500)) 
# If 'nlminb' failed, then try a few more optimization routines, 
#  quit when first one produces answer
Counter = 0
while(is.na(OptOutput$p1)){
  Counter = Counter + 1
  if(Counter == 1)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NBCFactors, 
    S = BCSc, NCMTs = NBaseCurve, MSR = MarketCMTRates, 
    method=c('BFGS'), control=list(save.failures=FALSE, maxit=2500)) 
  if(Counter == 2)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NBCFactors, 
    S = BCSc, NCMTs = NBaseCurve, MSR = MarketCMTRates, 
    method=c('Nelder-Mead'), control=list(save.failures=FALSE, maxit=2500))
  if(Counter == 3)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NBCFactors, 
    S = BCSc, NCMTs = NBaseCurve, MSR = MarketCMTRates, 
    method=c('L-BFGS-B'), control=list(save.failures=FALSE, maxit=2500)) 
}
# is.data.frame(OptOutput) # yes, it is
# x <- attr(OptOutput, "details")
OptMethod <- rownames(OptOutput[1]) # Method that provided answer, see Nash
y <- 0
y <- numeric(NBCFactors)
for(i in 1:NBCFactors){
  if(i==1)y[1] <- OptOutput$p1[1]
  if(i==2)y[2] <- OptOutput$p2[1]
  if(i==3)y[3] <- OptOutput$p3[1]
  if(i==4)y[4] <- OptOutput$p4[1]
  if(i==5)y[5] <- OptOutput$p5[1]
  if(i==6)y[6] <- OptOutput$p6[1]
  if(i==7)y[7] <- OptOutput$p7[1]
  if(i==8)y[8] <- OptOutput$p8[1]
}
LSCBaseCurveParameters <- y
# Check to see if sum of squared errors is close to zero
Answer2 <- DiffCMTRates(LSCBaseCurveParameters, NBCFactors, BCSc, NBaseCurve, 
                        MarketCMTRates)
Answer2
# Based on LSC parameters, y, and other inputs, NBCFactors, BCSc, NBaseCurve, 
#  provide estimates of fitted input rates
SREstimates <- CMTRates(LSCBaseCurveParameters, NBCFactors, BCSc, NBaseCurve)
SREstimates
# Based on LSC parameters, y, and other inputs, NBCFactors, BCSc, NBaseCurve, 
#  provide estimates of fitted discount rates
DREstimates <- DiscountRates(LSCBaseCurveParameters, NBCFactors, BCSc, 
                             NBaseCurve)
DREstimates
Maturity <- seq(1:NBaseCurve) # Maturity vector for plotting
#
# Plots: Range of y axis in input file
#
# Plot footers
NFs = paste0('Base Curve Factors = ', NBCFactors)
BCScs = paste0(', Base Curve Scalars = ')
if(NBCFactors==1){
  BCScs = paste0('')
  Ts = paste0('')
}
BCTau1R = round(BCTau[1],2)
if(NBCFactors==1)Ts = paste0("None")
if(NBCFactors==2)Ts = paste0(BCTau1R)
if(NBCFactors==3)Ts = paste0(BCTau1R)
if(NBCFactors==4)Ts = paste0(BCTau1R, ', ', BCTau[2])
if(NBCFactors==5)Ts = paste0(BCTau1R, ', ', BCTau[2], ', ', BCTau[3])
if(NBCFactors==6)Ts = paste0(BCTau1R, ', ', BCTau[2], ', ', BCTau[3], ', ', 
                             BCTau[4])
if(NBCFactors==7)Ts = paste0(BCTau1R, ', ', BCTau[2], ', ', BCTau[3], ', ', 
                             BCTau[4], ', ', BCTau[5])
if(NBCFactors==8)Ts = paste0(BCTau1R, ', ', BCTau[2], ', ', BCTau[3], ', ', 
                             BCTau[4], ', ', BCTau[5], ', ', BCTau[6])
sTitle = paste(NFs, BCScs, Ts)
MaxValue = max(Maturity, na.rm=TRUE)
MinValue = min(0.0, Maturity, na.rm=TRUE)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
if(FixRange){
  MaxValue = FRMax
  MinValue = FRMin
} else {
  MaxValue = max(MarketCMTRates, SREstimates, DREstimates, na.rm=TRUE)
  MinValue = min(MarketCMTRates, SREstimates, DREstimates, na.rm=TRUE)
}
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("CMT","LSC Base Curve Fit","Base Curve Discount Rates")
xTitle = "Maturity"
yTitle = "Rates"
lTitle = "Variable"
plot(Maturity, MarketCMTRates, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
     ylim = ylim1, pch = 1, cex = 0.5)
lines(Maturity, SREstimates, type = "b", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 2, cex = 0.5)
lines(Maturity, DREstimates, type = "b", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 3, cex = 0.5)
legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(2,2,2), 
       col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
#
# Spread 1 Analysis (BB curve, Bloomberg)
#
# NOTE: Inputs are all-in rates
source('SPREADS Functions.R') # Various Bond functions
#
# Work on spread curve
#
# Step 1: Estimate spreads based on LSC model base curve and all-in rates
#
# LSC base curve inputs
NumberOfFactors <- NBCFactors
Intercept <- 0
Slope <- 0
Curvature1 <- 0
Curvature2 <- 0
Curvature3 <- 0
Curvature4 <- 0
Curvature5 <- 0
Curvature6 <- 0
Intercept <- LSCBaseCurveParameters[1]
if(NumberOfFactors>1)Slope <- LSCBaseCurveParameters[2]
if(NumberOfFactors>2)Curvature1 <- LSCBaseCurveParameters[3]
if(NumberOfFactors>3)Curvature2 <- LSCBaseCurveParameters[4]
if(NumberOfFactors>4)Curvature3 <- LSCBaseCurveParameters[5]
if(NumberOfFactors>5)Curvature4 <- LSCBaseCurveParameters[6]
if(NumberOfFactors>6)Curvature5 <- LSCBaseCurveParameters[7]
if(NumberOfFactors>7)Curvature6 <- LSCBaseCurveParameters[8]
Tau1 <- 0
Tau2 <- 0
Tau3 <- 0
Tau4 <- 0
Tau5 <- 0
Tau6 <- 0
if(NumberOfFactors>1)Tau1 <- BCTau[1]
if(NumberOfFactors>3)Tau2 <- BCTau[2]
if(NumberOfFactors>4)Tau3 <- BCTau[3]
if(NumberOfFactors>5)Tau4 <- BCTau[4]
if(NumberOfFactors>6)Tau5 <- BCTau[5]
if(NumberOfFactors>7)Tau6 <- BCTau[6]
LSC <- list(Maturity, NumberOfFactors, Intercept, Slope, 
            Curvature1, Curvature2, Curvature3, Curvature4, Curvature5, Curvature6,
            Tau1, Tau2, Tau3, Tau4, Tau5, Tau6)
names(LSC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope",
                "Curvature1", "Curvature2", "Curvature3", "Curvature4", "Curvature5", 
                "Curvature6", "Tau1", "Tau2", "Tau3", "Tau4", "Tau5", "Tau6")
MarketSpreadRates <- MarketAllInRates
for(i in 1:NBaseCurve){
  LSC$Maturity <- i
  MarketSpreadRates[i] <- MarketAllInRates[i] - LSCRate(LSC)
}
MarketCMTRates
MarketAllInRates
MarketSpreadRates
# Step 2: Estimate spread curve
# SCTau[1] <- TimeToMaturity(BONDInputData) # Set first SCTau to be maturity
# x filled with initial guesses
x <- numeric(NSCFactors)   # b (Base curve level, slope, and curvatures)
if(NSCTau < 2)SCSc <- numeric(1)
if(NSCTau >= 2)SCSc <- numeric(NSCTau) # Base curve scalars
for(i in 1:NSCFactors){
  if(i==1){
    x[1] <- MarketSpreadRates[30]    # Level: Might be NA
    if(is.na(x[1]))x[1] <- 5.0     # Thus, set to 5%
    SCSc[1] <- 0.0
  }
  if(i==2){
    x[2] <- MarketSpreadRates[1] - MarketSpreadRates[30] # Slope
    if(is.na(x[2]))x[2] <- 0.0     # Defaults to zero
    SCSc[1] <- SCTau[1]
  }
  if(i>2){
    x[i] <- 0
    SCSc[i-2] <- SCTau[i-2]
  }
}
# Given coefficients for discount curve based on LSC, 
#  estimate sum squared difference
Answer <- DiffSwRates(x, NSCFactors, SCSc, NBaseCurve, MarketSpreadRates)
Answer
# optimx R package provides minimization routine to select LSC coefficients 
# to minimize squared differences #, all.methods=TRUE (uses all methods)
OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NSCFactors, S = SCSc, 
                    NCMTs = NBaseCurve, MSR = MarketSpreadRates, 
                    method=c('nlminb'), control=list(save.failures=FALSE, maxit=2500)) 
# If 'nlminb' failed, then try a few more optimization routines, 
#  quit when first one produces answer
Counter = 0
while(is.na(OptOutput$p1)){
  Counter = Counter + 1
  if(Counter == 1)OptOutput <- optimx(par=x, fn=DiffSwRates, 
                                      NSCFac = NSCFactors, S = SCSc, NCMTs = NBaseCurve, MSR = MarketSpreadRates, 
                                      method=c('BFGS'), control=list(save.failures=FALSE, maxit=2500)) 
  if(Counter == 2)OptOutput <- optimx(par=x, fn=DiffSwRates, 
                                      NSCFac = NSCFactors, S = SCSc, NCMTs = NBaseCurve, MSR = MarketSpreadRates, 
                                      method=c('Nelder-Mead'), control=list(save.failures=FALSE, maxit=2500))
  if(Counter == 3)OptOutput <- optimx(par=x, fn=DiffSwRates, 
                                      NSCFac = NSCFactors, S = SCSc, NCMTs = NBaseCurve, MSR = MarketSpreadRates, 
                                      method=c('L-BFGS-B'), control=list(save.failures=FALSE, maxit=2500)) 
}
# is.data.frame(OptOutput) # yes, it is
# x <- attr(OptOutput, "details")
OptMethod <- rownames(OptOutput[1]) # Method that provided answer, see Nash
ySC <- 0
ySC <- numeric(NSCFactors)
for(i in 1:NSCFactors){
  if(i==1)ySC[1] <- OptOutput$p1[1]
  if(i==2)ySC[2] <- OptOutput$p2[1]
  if(i==3)ySC[3] <- OptOutput$p3[1]
  if(i==4)ySC[4] <- OptOutput$p4[1]
  if(i==5)ySC[5] <- OptOutput$p5[1]
  if(i==6)ySC[6] <- OptOutput$p6[1]
  if(i==7)ySC[7] <- OptOutput$p7[1]
  if(i==8)ySC[8] <- OptOutput$p8[1]
}
LSCSCCurveParameters <- ySC
LSCBaseCurveParameters
LSCSCCurveParameters
# Check to see if sum of squared errors is close to zero
Answer2 <- DiffSwRates(LSCSCCurveParameters, NSCFactors, SCSc, 
                       NBaseCurve, MarketSpreadRates)
Answer2
# Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
#  provide estimates of fitted input rates
SRSpreadEstimates <- CMTRates(LSCSCCurveParameters, NSCFactors, SCSc, 
                              NBaseCurve)
SRSpreadEstimates
# Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
#  provide estimates of fitted discount rates
DRSpreadEstimates <- DiscountRates(LSCSCCurveParameters, NSCFactors, SCSc, 
                                   NBaseCurve)
DRSpreadEstimates
# Step 3: Compare with fitting all-in rate (Assume SC factors and scalars)
# optimx R package provides minimization routine to select LSC coefficients 
# to minimize squared differences #, all.methods=TRUE (uses all methods)
OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NSCFactors, S = SCSc, 
                    NCMTs = NBaseCurve, MSR = MarketAllInRates, 
                    method=c('nlminb'), control=list(save.failures=FALSE, maxit=2500)) 
# If 'nlminb' failed, then try a few more optimization routines, 
#  quit when first one produces answer
Counter = 0
while(is.na(OptOutput$p1)){
  Counter = Counter + 1
  if(Counter == 1)OptOutput <- optimx(par=x, fn=DiffSwRates, 
                                      NSCFac = NSCFactors, S = SCSc, NCMTs = NBaseCurve, MSR = MarketAllInRates, 
                                      method=c('BFGS'), control=list(save.failures=FALSE, maxit=2500)) 
  if(Counter == 2)OptOutput <- optimx(par=x, fn=DiffSwRates, 
                                      NSCFac = NSCFactors, S = SCSc, NCMTs = NBaseCurve, MSR = MarketAllInRates, 
                                      method=c('Nelder-Mead'), control=list(save.failures=FALSE, maxit=2500))
  if(Counter == 3)OptOutput <- optimx(par=x, fn=DiffSwRates, 
                                      NSCFac = NSCFactors, S = SCSc, NCMTs = NBaseCurve, MSR = MarketAllInRates, 
                                      method=c('L-BFGS-B'), control=list(save.failures=FALSE, maxit=2500)) 
}
# is.data.frame(OptOutput) # yes, it is
# x <- attr(OptOutput, "details")
OptMethod <- rownames(OptOutput[1]) # Method that provided answer, see Nash
yAllIn <- 0
yAllIn <- numeric(NSCFactors)
for(i in 1:NSCFactors){
  if(i==1)yAllIn[1] <- OptOutput$p1[1]
  if(i==2)yAllIn[2] <- OptOutput$p2[1]
  if(i==3)yAllIn[3] <- OptOutput$p3[1]
  if(i==4)yAllIn[4] <- OptOutput$p4[1]
  if(i==5)yAllIn[5] <- OptOutput$p5[1]
  if(i==6)yAllIn[6] <- OptOutput$p6[1]
  if(i==7)yAllIn[7] <- OptOutput$p7[1]
  if(i==8)yAllIn[8] <- OptOutput$p8[1]
}
LSCAllInCurveParameters <- yAllIn
LSCBaseCurveParameters
LSCAllInCurveParameters
# Check to see if sum of squared errors is close to zero
Answer3 <- DiffSwRates(LSCAllInCurveParameters, NSCFactors, SCSc, 
                       NBaseCurve, MarketAllInRates)
Answer3
# Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
#  provide estimates of fitted input rates
SRAllInEstimates <- CMTRates(LSCAllInCurveParameters, NSCFactors, SCSc, 
                             NBaseCurve)
SRAllInEstimates
# Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
#  provide estimates of fitted discount rates
DRAllInEstimates <- DiscountRates(LSCAllInCurveParameters, NSCFactors, SCSc, 
                                  NBaseCurve)
DRAllInEstimates
#
# Plots: Range of y axis in input file
#
Maturity <- seq(1:NBaseCurve) # Maturity vector for plotting
# Plot footers
NBCFs = paste0('BC Factors=', NBCFactors)
BCScs = paste0(', BC Scalars=')
if(NBCFactors==1){
  BCScs = paste0('')
  BCTs = paste0('')
}
BCTau1R = round(BCTau[1],2)
if(NBCFactors==1)BCTs = paste0("None")
if(NBCFactors==2)BCTs = paste0(BCTau1R)
if(NBCFactors==3)BCTs = paste0(BCTau1R)
if(NBCFactors==4)BCTs = paste0(BCTau1R, ', ', BCTau[2])
if(NBCFactors==5)BCTs = paste0(BCTau1R, ', ', BCTau[2], ', ', BCTau[3])
if(NBCFactors==6)BCTs = paste0(BCTau1R, ', ', BCTau[2], ', ', BCTau[3], ', ', 
                               BCTau[4])
if(NBCFactors==7)BCTs = paste0(BCTau1R, ', ', BCTau[2], ', ', BCTau[3], ', ', 
                               BCTau[4], ', ', BCTau[5])
if(NBCFactors==8)BCTs = paste0(BCTau1R, ', ', BCTau[2], ', ', BCTau[3], ', ', 
                               BCTau[4], ', ', BCTau[5], ', ', BCTau[6])
NSCFs = paste0('SC Factors=', NSCFactors)
SCScs = paste0(', SC Scalars=')
if(NSCFactors==1){
  SCScs = paste0('')
  SCTs = paste0('')
}
SCTau1R = round(SCTau[1],2)
if(NSCFactors==1)SCTs = paste0("None")
if(NSCFactors==2)SCTs = paste0(SCTau1R)
if(NSCFactors==3)SCTs = paste0(SCTau1R)
if(NSCFactors==4)SCTs = paste0(SCTau1R, ', ', SCTau[2])
if(NSCFactors==5)SCTs = paste0(SCTau1R, ', ', SCTau[2], ', ', SCTau[3])
if(NSCFactors==6)SCTs = paste0(SCTau1R, ', ', SCTau[2], ', ', SCTau[3], ', ', 
                               SCTau[4])
if(NSCFactors==7)SCTs = paste0(SCTau1R, ', ', SCTau[2], ', ', SCTau[3], ', ', 
                               SCTau[4], ', ', SCTau[5])
if(NSCFactors==8)SCTs = paste0(SCTau1R, ', ', SCTau[2], ', ', SCTau[3], ', ', 
                               SCTau[4], ', ', SCTau[5], ', ', SCTau[6])
sTitle = paste(NBCFs, BCScs, BCTs, NSCFs, SCScs, SCTs)
MaxValueX = max(Maturity, na.rm=TRUE)
MinValueX = min(0.0, Maturity, na.rm=TRUE)
xlim1 = c(1:2)
xlim1[1] = MinValueX
xlim1[2] = MaxValueX
if(FixRange){
  MaxValue = FRMax
  MinValue = FRMin
} else {
  MaxValue = max(MarketCMTRates, SREstimates, DREstimates, 
                 MarketAllInRates, SRAllInEstimates, DRAllInEstimates, na.rm=TRUE)
  MinValue = min(MarketCMTRates, SREstimates, DREstimates, 
                 MarketAllInRates, SRAllInEstimates, DRAllInEstimates, na.rm=TRUE)
}
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("CMT","CMT Fit","CMT DR","BB","BB Fit","BB DR")
mTitle = "UST and BB Yields"
xTitle = "Maturity"
yTitle = "Rates"
lTitle = "Variable"
plot(Maturity, MarketCMTRates, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
     ylim = ylim1, pch = 1, cex = 0.5)
lines(Maturity, SREstimates, type = "b", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 2, cex = 0.5)
lines(Maturity, DREstimates, type = "b", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 3, cex = 0.5)
lines(Maturity, MarketAllInRates, type = "p", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 4, cex = 0.5)
lines(Maturity, SRAllInEstimates, type = "b", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 5, cex = 0.5)
lines(Maturity, DRAllInEstimates, type = "b", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 6, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1,1,1), 
       lty = c(2,2,2,2,2,2), 
       col = c("black","black","black","black","black","black"), 
       pch = c(1,2,3,4,5,6), bty = "n", title = lTitle)
#
# Spot rate estimates graph
#
if(FixRange){
  MaxValue = FRMax
  MinValue = FRMin
} else {
  MaxValue = max(SREstimates, SRAllInEstimates, SRSpreadEstimates, na.rm=TRUE)
  MinValue = min(SREstimates, SRAllInEstimates, SRSpreadEstimates, na.rm=TRUE)
}
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("CMT Yields","BB Yields","Spread")
mTitle = "UST, BB Yields, and Spread"
xTitle = "Maturity"
yTitle = "Rates"
lTitle = "Variable"
plot(Maturity, SREstimates, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
     ylim = ylim1, pch = 1, cex = 0.5)
lines(Maturity, SRAllInEstimates, type = "b", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 2, cex = 0.5)
lines(Maturity, SRSpreadEstimates, type = "b", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(2,2,2), 
       col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
#
# Assessment of CMT and spread with selected bond
#
# Test inputs: BB Corporate Bond
ActualBondPrice = 100.6875            # Percent of par
BONDInputData$Frequency = 2L          # Coupon frequency per year, 1, 2, 4, or 12
BONDInputData$CouponRate = 5.5        # Percent
BONDInputData$Par = 100.0             # Currency
BONDInputData$YieldToMaturity = 5.41  # Percent
# Dollars: Quoted bond price without accrued interest (stubbed to -99)
BONDInputData$BondPrice = -99
BONDInputData$SettlementDateMonth = 6      # Integer: 1-12
BONDInputData$SettlementDateDay = 21       # Integer: 1-31
BONDInputData$SettlementDateYear = 2020    # Integer: 1-very high number
BONDInputData$MaturityDateMonth = 6        # Integer: 1-12
BONDInputData$MaturityDateDay = 21         # Integer: 1-31
BONDInputData$MaturityDateYear = 2030      # Integer: 1-very high number
# BONDInputData$ChangeInYTM = 0.01
TestCouponRemaining = CouponsRemaining(BONDInputData)
TestElapsed = Elapsed(BONDInputData)
TestElapsed <- TestElapsed$Fraction
TestAccruedInterest = AccruedInterest(BONDInputData)
TestCouponRemaining; TestElapsed; TestAccruedInterest
Maturity <- TimeToMaturity(BONDInputData)
#
# Work on filling BC and SC (LSC vectors with fitted LSC info)
#
BC <- LSC
SC <- BC # Initialize
SC$NumberOfFactors <- NSCFactors
SC$Intercept <- 0
SC$Slope <- 0
SC$Curvature1 <- 0
SC$Curvature2 <- 0
SC$Curvature3 <- 0
SC$Curvature4 <- 0
SC$Curvature5 <- 0
SC$Curvature6 <- 0
SC$Tau1 <- 0
SC$Tau2 <- 0
SC$Tau3 <- 0
SC$Tau4 <- 0
SC$Tau5 <- 0
SC$Tau6 <- 0
AI <- SC # Initialize all-in parameters
LengthSC <- length(LSCSCCurveParameters)
for(i in 1:LengthSC){
  if(i==1){
    SC$Intercept <- LSCSCCurveParameters[1]  
    AI$Intercept <- LSCAllInCurveParameters[1]
  }
  if(i==2){
    SC$Slope <- LSCSCCurveParameters[2]  
    SC$Tau1 <- SCTau[1]
    AI$Slope <- LSCAllInCurveParameters[2]  
    AI$Tau1 <- SCTau[1]
  }
  if(i==3){
    SC$Curvature1 <- LSCSCCurveParameters[3]  
    SC$Tau1 <- SCTau[1]
    AI$Curvature1 <- LSCAllInCurveParameters[3]  
    AI$Tau1 <- SCTau[1]
  }
  if(i==4){
    SC$Curvature2 <- LSCSCCurveParameters[4]  
    SC$Tau2 <- SCTau[2]
    AI$Curvature2 <- LSCAllInCurveParameters[4]  
    AI$Tau1 <- SCTau[2]
  }
  if(i==5){
    SC$Curvature3 <- LSCSCCurveParameters[5]  
    SC$Tau3 <- SCTau[3]
    AI$Curvature3 <- LSCAllInCurveParameters[5]  
    AI$Tau3 <- SCTau[3]
  }
  if(i==6){
    SC$Curvature4 <- LSCSCCurveParameters[6]  
    SC$Tau4 <- SCTau[4]
    AI$Curvature4 <- LSCAllInCurveParameters[6]  
    AI$Tau4 <- SCTau[4]
  }
}
# Analysis of different bond valuations
TestBondValue = BondValue(BONDInputData)
TestBondValueDF = BondValueDF(BONDInputData, AI)
TestBondValueDF2 = BondValueDF2(BONDInputData, BC, SC)
TestBondValue; TestBondValueDF; TestBondValueDF2
