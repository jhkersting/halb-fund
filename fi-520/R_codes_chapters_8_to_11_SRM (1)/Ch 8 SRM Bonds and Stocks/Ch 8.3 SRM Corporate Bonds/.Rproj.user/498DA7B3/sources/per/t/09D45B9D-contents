# SRM Spreads Over Base Curve Test.R
# rmarkdown::render("SRM Spreads Over Base Curve Test.R", "word_document")
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
inputChangeInYTM <- 0.01 # Effective duration and convexity
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
source("SRM BC and SC Analysis.R")







beep(sound = 2)



# # Select one bond for analysis (review valuation work)
# LengthCB <- length(CB$JMaturityDate) # Number of observations
# z <- FALSE # Indicator switch
# JTodaysDate <- as.integer(mdy.date(SettlementDateMonth, SettlementDateDay,
#   SettlementDateYear))
# for(i in 1:LengthCB){ # Find first bond in excess of RoughMaturity
#   TDate <- as.integer(CB$JMaturityDate[i])
#   TYears <- (as.numeric(TDate - JTodaysDate))/365.25
#   if(z == FALSE && TYears > RoughMaturity){
#     SelectedBond <- i
#     z <- TRUE
#   }
# }
# CB[SelectedBond,] # Console: Selected bond parameters
# inputCouponRate <- CB$COUPON[SelectedBond]
# inputYieldToMaturity <- CB$ASKED.YIELD[SelectedBond]
# inputBondPrice <- (CB$APrice[SelectedBond]/100.0)*inputPar
# MaturityDateMonth <- month(as.date(CB$JMaturityDate[SelectedBond]))
# MaturityDateDay <- day(as.date(CB$JMaturityDate[SelectedBond]))
# MaturityDateYear <- year(as.date(CB$JMaturityDate[SelectedBond]))
# BONDInputData <- list(inputFrequency, inputCouponRate, inputPar,
#   inputYieldToMaturity, inputBondPrice,
#   SettlementDateMonth, SettlementDateDay, SettlementDateYear,
#   MaturityDateMonth, MaturityDateDay, MaturityDateYear,
#   inputChangeInYTM)
# names(BONDInputData) <- c("Frequency", "CouponRate", "Par",
#   "YieldToMaturity", "BondPrice",
#   "SettlementDateMonth", "SettlementDateDay", "SettlementDateYear",
#   "MaturityDateMonth", "MaturityDateDay", "MaturityDateYear",
#   "ChangeInYTM")
# # Calendar manipulations
# N = CouponsRemaining(BONDInputData)
# # ElapsedOutput contains fraction, JLastDate, JNextDate, and JCurrentDate
# ElapsedOutput = Elapsed(BONDInputData)
# # Number of Total Days
# NTD <- ElapsedOutput$NextDate - ElapsedOutput$LastDate
# # Number of Accrued Days since last semi-annual coupon
# NAD <- ElapsedOutput$Fraction * NTD
# # Fraction of coupon period that has elapsed already
# f <- ElapsedOutput$Fraction
# # Bond maturity, in years
# Mat <- TimeToMaturity(BONDInputData)
# NAD; NTD; f; N; Mat
# # Bond value given yield to maturity
# MarketQuotedBondPrice <- inputBondPrice
# MarketValueOfBond <- BondValue(BONDInputData)
# AccruedInterestAmount <- AccruedInterest(BONDInputData)
# ModelQuotedBondPrice <- MarketValueOfBond - AccruedInterestAmount
# MarketValueOfBond; AccruedInterestAmount;
# ModelQuotedBondPrice; MarketQuotedBondPrice
# # Yield to maturity given bond value
# inputBondPrice = MarketQuotedBondPrice #Dollars:Quoted price w/o accrued interest
# BONDInputData$BondPrice <- inputBondPrice
# EstYieldToMaturity = YieldToMaturitySolver(BONDInputData)
# EstYieldToMaturity; inputYieldToMaturity
# #
# # Build zero coupon, annualized, cont. compounded, discount rate curve
# #  Inputs: NFactors - Number of factors
# #  NBaseCurve - Number of CMTs. MarketCMTRates=int. vector 1 to NBaseCurve
# # CMT curve information
# #
# source("CMT Inputs.R")
# # Access CB book
# source("Bond Inputs.R")
# # CB functions (semi-annual only)
# source("CB Functions.R")
# Tau[1] <- TimeToMaturity(BONDInputData) # Set first tau to be maturity
# # x filled with initial guesses
# x <- numeric(NFactors)   # b (Level, slope, and curvatures)
# if(NTau < 2)Sc <- numeric(1)
# if(NTau >= 2) Sc <- numeric(NTau) # Scalars
# for(i in 1:NFactors){
#   if(i==1){
#     x[1] <- MarketCMTRates[30]    # Level: Might be NA
#     if(is.na(x[1]))x[1] <- 5.0     # Thus, set to 5%
#     Sc[1] <- 0.0
#   }
#   if(i==2){
#     x[2] <- MarketCMTRates[1] - MarketCMTRates[30] # Slope
#     if(is.na(x[2]))x[2] <- 0.0     # Defaults to zero
#     Sc[1] <- Tau[1]
#   }
#   if(i>2){
#     x[i] <- 0
#     Sc[i-2] <- Tau[i-2]
#   }
# }
# # Just quickly check input parameters for DiffCMTRates
# x
# NFactors
# Sc
# NBaseCurve
# MarketCMTRates
# # Given coefficients for discount curve based on LSC, 
# #  estimate sum squared difference
# Answer <- DiffCMTRates(x, NFactors, Sc, NBaseCurve, MarketCMTRates)
# Answer
# # optimx R package provides minimization routine to select LSC coefficients 
# # to minimize squared differences #, all.methods=TRUE (uses all methods)
# OptOutput <- optimx(par=x, fn=DiffCMTRates, NFac = NFactors, S = Sc, 
#   NCMTs = NBaseCurve, MSR = MarketCMTRates, 
#   method=c('nlminb'), control=list(save.failures=FALSE, maxit=2500)) 
# # If 'nlminb' failed, then try a few more optimization routines, 
# #  quit when first one produces answer
# Counter = 0
# while(is.na(OptOutput$p1)){
#   Counter = Counter + 1
#   if(Counter == 1)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
#     S = Sc, NCMTs = NBaseCurve, MSR = MarketCMTRates, 
#     method=c('BFGS'), control=list(save.failures=FALSE, maxit=2500)) 
#   if(Counter == 2)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
#     S = Sc, NCMTs = NBaseCurve, MSR = MarketCMTRates, 
#     method=c('Nelder-Mead'), control=list(save.failures=FALSE, maxit=2500))
#   if(Counter == 3)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
#     S = Sc, NCMTs = NBaseCurve, MSR = MarketCMTRates, 
#     method=c('L-BFGS-B'), control=list(save.failures=FALSE, maxit=2500)) 
# }
# # is.data.frame(OptOutput) # yes, it is
# # x <- attr(OptOutput, "details")
# OptMethod <- rownames(OptOutput[1]) # Method that provided answer, see Nash
# y <- 0
# y <- numeric(NFactors)
# for(i in 1:NFactors){
#   if(i==1)y[1] <- OptOutput$p1[1]
#   if(i==2)y[2] <- OptOutput$p2[1]
#   if(i==3)y[3] <- OptOutput$p3[1]
#   if(i==4)y[4] <- OptOutput$p4[1]
#   if(i==5)y[5] <- OptOutput$p5[1]
#   if(i==6)y[6] <- OptOutput$p6[1]
#   if(i==7)y[7] <- OptOutput$p7[1]
#   if(i==8)y[8] <- OptOutput$p8[1]
# }
# LSCBaseCurveParameters <- y
# # Check to see if sum of squared errors is close to zero
# Answer2 <- DiffCMTRates(LSCBaseCurveParameters, NFactors, Sc, NBaseCurve, MarketCMTRates)
# Answer2
# # Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
# #  provide estimates of fitted input rates
# SREstimates <- CMTRates(LSCBaseCurveParameters, NFactors, Sc, NBaseCurve)
# SREstimates
# # Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
# #  provide estimates of fitted discount rates
# DREstimates <- DiscountRates(LSCBaseCurveParameters, NFactors, Sc, NBaseCurve)
# DREstimates
# Maturity <- seq(1:NBaseCurve) # Maturity vector for plotting



# #
# # Plots: Range of y axis in input file
# #
# # Plot footers
# NFs = paste0('LSC Factors = ', NBCFactors)
# Scs = paste0(', Scalars = ')
# if(NBCFactors==1){
#   Scs = paste0('')
#   Ts = paste0('')
# }
# Tau1R = round(Tau[1],2)
# if(NFactors==1)Ts = paste0("None")
# if(NFactors==2)Ts = paste0(Tau1R)
# if(NFactors==3)Ts = paste0(Tau1R)
# if(NFactors==4)Ts = paste0(Tau1R, ', ', Tau[2])
# if(NFactors==5)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3])
# if(NFactors==6)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3], ', ', Tau[4])
# if(NFactors==7)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3], ', ', Tau[4],
#   ', ', Tau[5])
# if(NFactors==8)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3], ', ', Tau[4], 
#   ', ', Tau[5], ', ', Tau[6])
# sTitle = paste(NFs, Scs, Ts)
# MaxValue = max(Maturity, na.rm=TRUE)
# MinValue = min(0.0, Maturity, na.rm=TRUE)
# xlim1 = c(1:2)
# xlim1[1] = MinValue
# xlim1[2] = MaxValue
# if(FixRange){
#   MaxValue = FRMax
#   MinValue = FRMin
# } else {
#   MaxValue = max(MarketCMTRates, SREstimates, DREstimates, na.rm=TRUE)
#   MinValue = min(MarketCMTRates, SREstimates, DREstimates, na.rm=TRUE)
# }
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("CMT","LSC Base Curve Fit","Base Curve Discount Rates")
# xTitle = "Maturity"
# yTitle = "Rates"
# lTitle = "Variable"
# plot(Maturity, MarketCMTRates, type = "p", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(Maturity, SREstimates, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(Maturity, DREstimates, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(2,2,2), 
#   col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
# #
# # Spread 1 Analysis (BB curve, Bloomberg)
# #
# source('SPREADS Functions.R') # Various Bond functions
# OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, S = Sc, 
#   NCMTs = NBaseCurve, MSR = MarketAllInRates, 
#   method=c('nlminb'), control=list(save.failures=FALSE, maxit=2500)) 
# # If 'nlminb' failed, then try a few more optimization routines, 
# #  quit when first one produces answer
# Counter = 0
# while(is.na(OptOutput$p1)){
#   Counter = Counter + 1
#   if(Counter == 1)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
#     S = Sc, NCMTs = NBaseCurve, MSR = MarketAllInRates, 
#     method=c('BFGS'), control=list(save.failures=FALSE, maxit=2500)) 
#   if(Counter == 2)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
#     S = Sc, NCMTs = NBaseCurve, MSR = MarketAllInRates, 
#     method=c('Nelder-Mead'), control=list(save.failures=FALSE, maxit=2500))
#   if(Counter == 3)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
#     S = Sc, NCMTs = NBaseCurve, MSR = MarketAllInRates, 
#     method=c('L-BFGS-B'), control=list(save.failures=FALSE, maxit=2500)) 
# }
# # is.data.frame(OptOutput) # yes, it is
# # x <- attr(OptOutput, "details")
# OptMethod <- rownames(OptOutput[1]) # Method that provided answer, see Nash
# yAllIn <- 0
# yAllIn <- numeric(NFactors)
# for(i in 1:NFactors){
#   if(i==1)yAllIn[1] <- OptOutput$p1[1]
#   if(i==2)yAllIn[2] <- OptOutput$p2[1]
#   if(i==3)yAllIn[3] <- OptOutput$p3[1]
#   if(i==4)yAllIn[4] <- OptOutput$p4[1]
#   if(i==5)yAllIn[5] <- OptOutput$p5[1]
#   if(i==6)yAllIn[6] <- OptOutput$p6[1]
#   if(i==7)yAllIn[7] <- OptOutput$p7[1]
#   if(i==8)yAllIn[8] <- OptOutput$p8[1]
# }
# LSCAllInCurveParameters <- yAllIn
# LSCBaseCurveParameters
# LSCAllInCurveParameters
# LSCSpreadCurveParameters <- LSCAllInCurveParameters - LSCBaseCurveParameters
# LSCSpreadCurveParameters
# # Check to see if sum of squared errors is close to zero
# Answer2 <- DiffSwRates(LSCAllInCurveParameters, NFactors, Sc, NBaseCurve, MarketCMTRates)
# Answer2
# # Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
# #  provide estimates of fitted input rates
# SRAllInEstimates <- CMTRates(LSCAllInCurveParameters, NFactors, Sc, NBaseCurve)
# SRAllInEstimates
# # Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
# #  provide estimates of fitted discount rates
# DRAllInEstimates <- DiscountRates(LSCAllInCurveParameters, NFactors, Sc, NBaseCurve)
# DRAllInEstimates
# # Credit spread only
# SRSCEstimates <- CMTRates(LSCSpreadCurveParameters, NFactors, Sc, NBaseCurve)
# SRSCEstimates
# #
# # Plots: Range of y axis in input file
# #
# Maturity <- seq(1:NBaseCurve) # Maturity vector for plotting
# # Plot footers
# NFs = paste0('LSC Factors = ', NFactors)
# Scs = paste0(', Scalars = ')
# if(NFactors==1){
#   Scs = paste0('')
#   Ts = paste0('')
# }
# Tau1R = round(Tau[1],2)
# if(NFactors==1)Ts = paste0("None")
# if(NFactors==2)Ts = paste0(Tau1R)
# if(NFactors==3)Ts = paste0(Tau1R)
# if(NFactors==4)Ts = paste0(Tau1R, ', ', Tau[2])
# if(NFactors==5)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3])
# if(NFactors==6)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3], ', ', Tau[4])
# if(NFactors==7)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3], ', ', Tau[4],
#   ', ', Tau[5])
# if(NFactors==8)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3], ', ', Tau[4], 
#   ', ', Tau[5], ', ', Tau[6])
# sTitle = paste(NFs, Scs, Ts)
# MaxValueX = max(Maturity, na.rm=TRUE)
# MinValueX = min(0.0, Maturity, na.rm=TRUE)
# xlim1 = c(1:2)
# xlim1[1] = MinValueX
# xlim1[2] = MaxValueX
# if(FixRange){
#   MaxValue = FRMax
#   MinValue = FRMin
# } else {
#   MaxValue = max(MarketCMTRates, SREstimates, DREstimates, 
#     MarketAllInRates, SRAllInEstimates, DRAllInEstimates, na.rm=TRUE)
#   MinValue = min(MarketCMTRates, SREstimates, DREstimates, 
#     MarketAllInRates, SRAllInEstimates, DRAllInEstimates, na.rm=TRUE)
# }
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("CMT","CMT Fit","CMT DR","BB","BB Fit","BB DR")
# mTitle = "UST and BB Yields"
# xTitle = "Maturity"
# yTitle = "Rates"
# lTitle = "Variable"
# plot(Maturity, MarketCMTRates, type = "p", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(Maturity, SREstimates, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(Maturity, DREstimates, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 3, cex = 0.5)
# lines(Maturity, MarketAllInRates, type = "p", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 4, cex = 0.5)
# lines(Maturity, SRAllInEstimates, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 5, cex = 0.5)
# lines(Maturity, DRAllInEstimates, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 6, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1,1,1), 
#   lty = c(2,2,2,2,2,2), 
#   col = c("black","black","black","black","black","black"), 
#   pch = c(1,2,3,4,5,6), bty = "n", title = lTitle)
# if(FixRange){
#   MaxValue = FRMax
#   MinValue = FRMin
# } else {
#   MaxValue = max(SREstimates, SRAllInEstimates, SRSCEstimates, na.rm=TRUE)
#   MinValue = min(SREstimates, SRAllInEstimates, SRSCEstimates, na.rm=TRUE)
# }
# ylim1 = c(1:2)
# ylim1[1] = MinValue
# ylim1[2] = MaxValue
# legtxt = c("CMT Yields","BB Yields","Spread")
# mTitle = "UST, BB Yields, and Spread"
# xTitle = "Maturity"
# yTitle = "Rates"
# lTitle = "Variable"
# plot(Maturity, SREstimates, type = "p", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(Maturity, SRAllInEstimates, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(Maturity, SRSCEstimates, type = "b", col ="black", xlim = xlim1, 
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(2,2,2), 
#   col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
# #
# # Assessment of CMT and spread with selected bond
# #
# # Test inputs: BB Corporate Bond
# ActualBondPrice = 100.6875            # Percent of par
# BONDInputData$Frequency = 2L          # Coupon frequency per year, 1, 2, 4, or 12
# BONDInputData$CouponRate = 5.5        # Percent
# BONDInputData$Par = 100.0             # Currency
# BONDInputData$YieldToMaturity = 5.41  # Percent
# # Dollars: Quoted bond price without accrued interest (stubbed to -99)
# BONDInputData$BondPrice = -99       
# BONDInputData$SettlementDateMonth = 6      # Integer: 1-12
# BONDInputData$SettlementDateDay = 21       # Integer: 1-31
# BONDInputData$SettlementDateYear = 2020    # Integer: 1-very high number
# BONDInputData$MaturityDateMonth = 6        # Integer: 1-12
# BONDInputData$MaturityDateDay = 21         # Integer: 1-31
# BONDInputData$MaturityDateYear = 2030      # Integer: 1-very high number
# BONDInputData$ChangeInYTM = 0.01
# TestCouponRemaining = CouponsRemaining(BONDInputData)
# TestElapsed = Elapsed(BONDInputData)
# TestElapsed <- TestElapsed$Fraction
# TestAccruedInterest = AccruedInterest(BONDInputData)
# TestCouponRemaining; TestElapsed; TestAccruedInterest
# Maturity <- TimeToMaturity(BONDInputData)
# NumberOfFactors <- NFactors
# Intercept <- 0
# Slope <- 0
# Curvature1 <- 0
# Curvature2 <- 0
# Curvature3 <- 0
# Curvature4 <- 0
# Curvature5 <- 0
# Curvature6 <- 0
# Intercept <- LSCAllInCurveParameters[1]
# if(NumberOfFactors>1)Slope <- LSCAllInCurveParameters[2]
# if(NumberOfFactors>2)Curvature1 <- LSCAllInCurveParameters[3]
# if(NumberOfFactors>3)Curvature2 <- LSCAllInCurveParameters[4]
# if(NumberOfFactors>4)Curvature3 <- LSCAllInCurveParameters[5]
# if(NumberOfFactors>5)Curvature4 <- LSCAllInCurveParameters[6]
# if(NumberOfFactors>6)Curvature5 <- LSCAllInCurveParameters[7]
# if(NumberOfFactors>7)Curvature6 <- LSCAllInCurveParameters[8]
# Tau1 <- 0
# Tau2 <- 0
# Tau3 <- 0
# Tau4 <- 0
# Tau5 <- 0
# Tau6 <- 0
# if(NumberOfFactors>1)Tau1 <- Tau[1]
# if(NumberOfFactors>3)Tau2 <- Tau[2]
# if(NumberOfFactors>4)Tau3 <- Tau[3]
# if(NumberOfFactors>5)Tau4 <- Tau[4]
# if(NumberOfFactors>6)Tau5 <- Tau[5]
# if(NumberOfFactors>7)Tau6 <- Tau[6]
# LSC <- list(Maturity, NumberOfFactors, Intercept, Slope, 
#   Curvature1, Curvature2, Curvature3, Curvature4, Curvature5, Curvature6,
#   Tau1, Tau2, Tau3, Tau4, Tau5, Tau6)
# names(LSC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope",
#   "Curvature1", "Curvature2", "Curvature3", "Curvature4", "Curvature5", 
#   "Curvature6", "Tau1", "Tau2", "Tau3", "Tau4", "Tau5", "Tau6")
# # Analysis of bond differential
# TestBondValue = BondValue(BONDInputData)
# TestBondValueDF = BondValueDF(BONDInputData, LSC)
# RelativeBVError = log(TestBondValueDF/TestBondValue)*100
# AbsoluteBVError = TestBondValueDF - TestBondValue
# TestBondValue; TestBondValueDF; RelativeBVError; AbsoluteBVError
# # Analysis of yield differential
# BONDInputData$BondPrice = TestBondValueDF
# TestYieldToMaturityDF = YieldToMaturitySolver(BONDInputData)
# YieldDiffBPs <- (BONDInputData$YieldToMaturity - TestYieldToMaturityDF)*100
# BONDInputData$YieldToMaturity; TestYieldToMaturityDF; YieldDiffBPs; Maturity
# 
# 
# 
# #
# # Run on CB bond set: Align CMT w bond quotes
# #
# LengthCB <- length(CB$JMaturityDate)
# TestBondValueDF <- numeric(LengthCB)
# RelativeBVError <- numeric(LengthCB)
# AbsoluteBVError <- numeric(LengthCB)
# TestYieldToMaturityDF <- numeric(LengthCB)
# YieldDiffBPs <- numeric(LengthCB)
# Maturity <- numeric(LengthCB) 
# MacDuration <- numeric(LengthCB) 
# StdConvexity <- numeric(LengthCB) 
# EffDuration <- numeric(LengthCB) 
# EffConvexity <- numeric(LengthCB) 
# for(i in 1:LengthCB){
#   BONDInputData$CouponRate <- CB$COUPON[i]
#   BONDInputData$YieldToMaturity <- CB$ASKED.YIELD[i] + CB$Spread[i]
#   BONDInputData$BondPrice <- CB$APrice[i]
#   BONDInputData$MaturityDateMonth <- month(as.date(CB$JMaturityDate[i]))
#   BONDInputData$MaturityDateDay <- day(as.date(CB$JMaturityDate[i])) 
#   BONDInputData$MaturityDateYear <- year(as.date(CB$JMaturityDate[i]))
#   Maturity[i] <- TimeToMaturity(BONDInputData)
#   LSC$Maturity <- Maturity[i]
#   TestBondValueDF[i] = BondValueDF(BONDInputData, LSC)
#   BONDInputData$BondPrice = TestBondValueDF[i] - AccruedInterest(BONDInputData)
#   RelativeBVError[i] = log(TestBondValueDF[i]/BONDInputData$BondPrice)*100
#   AbsoluteBVError[i] = ((TestBondValueDF[i] - BONDInputData$BondPrice)/inputPar)*100
#   TestYieldToMaturityDF[i] = YieldToMaturitySolver(BONDInputData)
# # Static risk management output
#   MacDuration[i] = Duration(BONDInputData)  # Macaulay duration
#   StdConvexity[i] = Convexity(BONDInputData) # Standard convexity
#   EffDuration[i] = EffectiveDuration(BONDInputData)
#   EffConvexity[i] = EffectiveConvexity(BONDInputData)
# }
# x <- Maturity
# xTitle = "Maturity"
# y <- CB$ASKED.YIELD + CB$Spread
# yTitle = "Asked Yield to Maturity"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle,
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# x <- MacDuration
# xTitle = "Macaulay Duration"
# y <- CB$ASKED.YIELD + CB$Spread
# yTitle = "Asked Yield to Maturity"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle,
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# 
# x <- MacDuration
# xTitle = "Macaulay Duration"
# y <- RelativeBVError
# yTitle = "Relative Bond Value Error"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle,
#      ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# # Effective duration and maturity
# x <- Maturity
# xTitle = "Maturity"
# y <- EffDuration
# yTitle = "Effective Duration"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle,
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# # Effective convexity and maturity
# x <- Maturity
# xTitle = "Maturity"
# y <- EffConvexity
# yTitle = "Effective Convexity"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle,
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)
# # Effective duration and effective convexity
# x <- EffDuration
# xTitle = "Effective Duration"
# y <- StdConvexity
# yTitle = "Effective Convexity"
# MaxXValue = max(x, na.rm=TRUE)
# MinXValue = min(0.0, x, na.rm=TRUE)
# xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MaxYValue = max(y, na.rm=TRUE)
# MinYValue = min(y, na.rm=TRUE)
# ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
# plot(x, y, type = "p", main = mTitle, sub = sTitle, xlab = xTitle,
#   ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex=0.5)


