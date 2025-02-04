# SRM LSC Factors.R

# Analysis of different bond durations
TestMD = EffectiveDuration(BONDInputData)
TestAllInCurveMDLevel = LSCMDLevel(BONDInputData, AI)
TestBaseCurveMDLevel = BaseCurveMDLevel(BONDInputData, BC, SC)
TestSpreadCurveMDLevel = SpreadCurveMDLevel(BONDInputData, BC, SC)
TestMD; TestAllInCurveMDLevel; TestBaseCurveMDLevel;TestSpreadCurveMDLevel
TestAllInCurveMDSlope = LSCMDSlope(BONDInputData, AI)
TestBaseCurveMDSlope = BaseCurveMDSlope(BONDInputData, BC, SC)
TestSpreadCurveMDSlope = SpreadCurveMDSlope(BONDInputData, BC, SC)
TestAllInCurveMDSlope; TestBaseCurveMDSlope; TestSpreadCurveMDSlope
TestAllInCurveMDCurvature1 = LSCMDCurvature1(BONDInputData, AI)
TestBaseCurveMDCurvature1 = BaseCurveMDCurvature1(BONDInputData, BC, SC)
TestSpreadCurveMDCurvature1 = SpreadCurveMDCurvature1(BONDInputData, BC, SC)
TestAllInCurveMDCurvature1; TestBaseCurveMDCurvature1;TestSpreadCurveMDCurvature1
# Analysis of different bond convexities
TestCY = EffectiveConvexity(BONDInputData)
TestAllInCurveCYLevel = LSCCYLevel(BONDInputData, AI)
TestBaseCurveCYLevel = BaseCurveCYLevel(BONDInputData, BC, SC)
TestSpreadCurveCYLevel = SpreadCurveCYLevel(BONDInputData, BC, SC)
TestCY; TestAllInCurveCYLevel; TestBaseCurveCYLevel;TestSpreadCurveCYLevel
BONDInputData$ChangeInYTM <- BONDInputData$ChangeInYTM*10 # Need larger increment
TestAllInCurveCYSlope = LSCCYSlope(BONDInputData, AI)
TestBaseCurveCYSlope = BaseCurveCYSlope(BONDInputData, BC, SC)
TestSpreadCurveCYSlope = SpreadCurveCYSlope(BONDInputData, BC, SC)
TestAllInCurveCYSlope; TestBaseCurveCYSlope; TestSpreadCurveCYSlope
TestAllInCurveCYCurvature1 = LSCCYCurvature1(BONDInputData, AI)
TestBaseCurveCYCurvature1 = BaseCurveCYCurvature1(BONDInputData, BC, SC)
TestSpreadCurveCYCurvature1 = SpreadCurveCYCurvature1(BONDInputData, BC, SC)
TestAllInCurveCYCurvature1; TestBaseCurveCYCurvature1; TestSpreadCurveCYCurvature1
BONDInputData$ChangeInYTM <- BONDInputData$ChangeInYTM/10 # Reset back
# Analysis of different cross-convexities
TestAllInCurveCCLevelSlope = LSCCCLevelSlope(BONDInputData, AI)
TestBaseCurveCCLevelSlope = BaseCurveCCLevelSlope(BONDInputData, BC, SC)
TestSpreadCurveCCLevelSlope = SpreadCurveCCLevelSlope(BONDInputData, BC, SC)
TestAllInCurveCCLevelSlope; TestBaseCurveCCLevelSlope; TestSpreadCurveCCLevelSlope
TestAllInCurveCCLevelCurvature1 = LSCCCLevelCurvature1(BONDInputData, AI)
TestBaseCurveCCLevelCurvature1 = BaseCurveCCLevelCurvature1(BONDInputData, BC, SC)
TestSpreadCurveCCLevelCurvature1 = SpreadCurveCCLevelCurvature1(BONDInputData, BC, SC)
TestAllInCurveCCLevelCurvature1; TestBaseCurveCCLevelCurvature1; TestSpreadCurveCCLevelCurvature1
TestAllInCurveCCSlopeCurvature1 = LSCCCSlopeCurvature1(BONDInputData, AI)
TestBaseCurveCCSlopeCurvature1 = BaseCurveCCSlopeCurvature1(BONDInputData, BC, SC)
TestSpreadCurveCCSlopeCurvature1 = SpreadCurveCCSlopeCurvature1(BONDInputData, BC, SC)
TestAllInCurveCCSlopeCurvature1; TestBaseCurveCCSlopeCurvature1; TestSpreadCurveCCSlopeCurvature1


#
# Run on CB bond set: Align CMT w bond quotes
#
LengthCB <- length(CB$JMaturityDate)
TestBondValueDF <- numeric(LengthCB)
RelativeBVError <- numeric(LengthCB)
AbsoluteBVError <- numeric(LengthCB)
TestYieldToMaturityDF <- numeric(LengthCB)
YieldToMaturity <- numeric(LengthCB)
TestBondValueDF2 <- numeric(LengthCB)
RelativeBVError2 <- numeric(LengthCB)
AbsoluteBVError2 <- numeric(LengthCB)
# TestYieldToMaturityDF2 <- numeric(LengthCB)
YieldDiffBPs <- numeric(LengthCB)
Maturity <- numeric(LengthCB)
MacDuration <- numeric(LengthCB)
StdConvexity <- numeric(LengthCB)
EffDuration <- numeric(LengthCB)
EffConvexity <- numeric(LengthCB)
# LSC Output Variables
VMkt0 <- numeric(LengthCB) # Market Ask Value at time 0
VMdl0 <- numeric(LengthCB) # Model Estimated Value at time 0
VMdlH <- numeric(LengthCB) # Model Estimated Value at Horizon
HPRH <- numeric(LengthCB) # Holding period return, horizon
AIMDLevel <- numeric(LengthCB)
AIMDSlope <- numeric(LengthCB)
AIMDCurvature1 <- numeric(LengthCB)
AICYLevel <- numeric(LengthCB)
AICYSlope <- numeric(LengthCB)
AICYCurvature1 <- numeric(LengthCB)
AICCLevelSlope <- numeric(LengthCB)
AICCLevelCurvature1 <- numeric(LengthCB)
AICCSlopeCurvature1 <- numeric(LengthCB)
# Analysis based on two LSC fitted curves
VMdl02 <- numeric(LengthCB) # Model Estimated Value at time 0
VMdlH2 <- numeric(LengthCB) # Model Estimated Value at Horizon
HPRH2 <- numeric(LengthCB) # Holding period return, horizon
# Base curve
BCMDLevel <- numeric(LengthCB)
BCMDSlope <- numeric(LengthCB)
BCMDCurvature1 <- numeric(LengthCB)
BCCYLevel <- numeric(LengthCB)
BCCYSlope <- numeric(LengthCB)
BCCYCurvature1 <- numeric(LengthCB)
BCCCLevelSlope <- numeric(LengthCB)
BCCCLevelCurvature1 <- numeric(LengthCB)
BCCCSlopeCurvature1 <- numeric(LengthCB)
# Spread curve
SCMDLevel <- numeric(LengthCB)
SCMDSlope <- numeric(LengthCB)
SCMDCurvature1 <- numeric(LengthCB)
SCCYLevel <- numeric(LengthCB)
SCCYSlope <- numeric(LengthCB)
SCCYCurvature1 <- numeric(LengthCB)
SCCCLevelSlope <- numeric(LengthCB)
SCCCLevelCurvature1 <- numeric(LengthCB)
SCCCSlopeCurvature1 <- numeric(LengthCB)
OriginalSettlementDateMonth <- BONDInputData$SettlementDateMonth
OriginalSettlementDateDay <- BONDInputData$SettlementDateDay
OriginalSettlementDateYear <- BONDInputData$SettlementDateYear
for(i in 1:LengthCB){
  BONDInputData$CouponRate <- CB$COUPON[i]
  BONDInputData$YieldToMaturity <- CB$ASKED.YIELD[i] + CB$Spread[i]
  BONDInputData$BondPrice <- CB$APrice[i]
  BONDInputData$MaturityDateMonth <- month(as.date(CB$JMaturityDate[i]))
  BONDInputData$MaturityDateDay <- day(as.date(CB$JMaturityDate[i]))
  BONDInputData$MaturityDateYear <- year(as.date(CB$JMaturityDate[i]))
  Maturity[i] <- TimeToMaturity(BONDInputData)
  AI$Maturity <- Maturity[i]
  BC$Maturity <- Maturity[i]
  SC$Maturity <- Maturity[i]
  TestBondValueDF[i] = BondValueDF(BONDInputData, AI)
  TestBondValueDF2[i] = BondValueDF2(BONDInputData, BC, SC)
  BONDInputData$BondPrice = TestBondValueDF[i] - AccruedInterest(BONDInputData)
  RelativeBVError[i] = log(TestBondValueDF[i]/BONDInputData$BondPrice)*100
  AbsoluteBVError[i] = ((TestBondValueDF[i] - BONDInputData$BondPrice)/inputPar)*100
  RelativeBVError2[i] = log(TestBondValueDF2[i]/BONDInputData$BondPrice)*100
  AbsoluteBVError2[i] = ((TestBondValueDF2[i] - BONDInputData$BondPrice)/inputPar)*100
  YieldToMaturity[i] = YieldToMaturitySolver(BONDInputData)
  # Static risk management output
  MacDuration[i] = Duration(BONDInputData)  # Macaulay duration
  StdConvexity[i] = Convexity(BONDInputData) # Standard convexity
  EffDuration[i] = EffectiveDuration(BONDInputData)
  EffConvexity[i] = EffectiveConvexity(BONDInputData)
  # AI Analysis
  VMkt0[i] <- CB$APrice[i] + AccruedInterest(BONDInputData)
  VMdl0[i] <- BondValueDF(BONDInputData, AI)
  VMdl02[i] <- BondValueDF2(BONDInputData, BC, SC)
  JTodaysDate <- as.integer(mdy.date(BONDInputData$SettlementDateMonth,
    BONDInputData$SettlementDateDay, BONDInputData$SettlementDateYear))
  HJDate <- as.date(JTodaysDate + inputHorizon)
  BONDInputData$SettlementDateMonth <- month(HJDate)
  BONDInputData$SettlementDateDay <- day(HJDate)
  BONDInputData$SettlementDateYear <- year(HJDate)
  VMdlH[i] <- BondValueDF(BONDInputData, AI)
  VMdlH2[i] <- BondValueDF2(BONDInputData,  BC, SC)
  # HPR horizon is zero for single coupon as rates advanced set
  HPRH[i] <- ((VMdlH[i] - VMdl0[i])/VMdl0[i])*100.0
  HPRH2[i] <- ((VMdlH2[i] - VMdl02[i])/VMdl02[i])*100.0
# AI SRMs
  AIMDLevel[i] <- LSCMDLevel(BONDInputData, AI)
  AIMDSlope[i] <- LSCMDSlope(BONDInputData, AI)
  AIMDCurvature1[i] <- LSCMDCurvature1(BONDInputData, AI)
  AICYLevel[i] <- LSCCYLevel(BONDInputData, AI)
  AICYSlope[i] <- LSCCYSlope(BONDInputData, AI)
  AICYCurvature1[i] <- LSCCYCurvature1(BONDInputData, AI)
  AICCLevelSlope[i] <- LSCCCLevelSlope(BONDInputData, AI)
  AICCLevelCurvature1[i] <- LSCCCLevelCurvature1(BONDInputData, AI)
  AICCSlopeCurvature1[i] <- LSCCCSlopeCurvature1(BONDInputData, AI)
# BC SRMs
  BCMDLevel[i] <-BaseCurveMDLevel(BONDInputData, BC, SC)
  BCMDSlope[i] <- BaseCurveMDSlope(BONDInputData, BC, SC)
  BCMDCurvature1[i] <- BaseCurveMDCurvature1(BONDInputData, BC, SC)
  BCCYLevel[i] <- BaseCurveCYLevel(BONDInputData, BC, SC)
  BCCYSlope[i] <- BaseCurveCYSlope(BONDInputData, BC, SC)
  BCCYCurvature1[i] <- BaseCurveCYCurvature1(BONDInputData, BC, SC)
  BCCCLevelSlope[i] <- BaseCurveCCLevelSlope(BONDInputData, BC, SC)
  BCCCLevelCurvature1[i] <- BaseCurveCCLevelCurvature1(BONDInputData, BC, SC)
  BCCCSlopeCurvature1[i] <- BaseCurveCCSlopeCurvature1(BONDInputData, BC, SC)
# SC SRMs
  SCMDLevel[i] <- SpreadCurveMDLevel(BONDInputData, BC, SC)
  SCMDSlope[i] <- SpreadCurveMDSlope(BONDInputData, BC, SC)
  SCMDCurvature1[i] <- SpreadCurveMDCurvature1(BONDInputData, BC, SC)
  SCCYLevel[i] <- SpreadCurveCYLevel(BONDInputData, BC, SC)
  SCCYSlope[i] <- SpreadCurveCYSlope(BONDInputData, BC, SC)
  SCCYCurvature1[i] <- SpreadCurveCYCurvature1(BONDInputData, BC, SC)
  SCCCLevelSlope[i] <- SpreadCurveCCLevelSlope(BONDInputData, BC, SC)
  SCCCLevelCurvature1[i] <- SpreadCurveCCLevelCurvature1(BONDInputData, BC, SC)
  SCCCSlopeCurvature1[i] <- SpreadCurveCCSlopeCurvature1(BONDInputData, BC, SC)
  BONDInputData$SettlementDateMonth <- OriginalSettlementDateMonth
  BONDInputData$SettlementDateDay <- OriginalSettlementDateDay
  BONDInputData$SettlementDateYear <- OriginalSettlementDateYear
}
