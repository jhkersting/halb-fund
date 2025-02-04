# SRM UST LSC Functions.R
#  Numerical derivatives
#
# LSC Modified Duration -- Level
#
LSCMDLevel = function(B, LSC){
  OriginalLevel = LSC$Intercept
  OriginalBV = BondValueDF(B, LSC)
  LSC$Intercept = OriginalLevel + B$ChangeInYTM
  UpBV = BondValueDF(B, LSC)
  LSC$Intercept = OriginalLevel - B$ChangeInYTM
  DownBV = BondValueDF(B, LSC)
  LSC$Intercept = OriginalLevel
  BCMDLevel = (DownBV - UpBV)/(2.0*OriginalBV*(B$ChangeInYTM/100.0))
  return( BCMDLevel ) 
}
#
# LSC Modified Duration -- Slope
#
LSCMDSlope = function(B, LSC){
  OriginalSlope = LSC$Slope
  OriginalBV = BondValueDF(B, LSC)
  LSC$Slope = OriginalSlope + B$ChangeInYTM
  UpBV = BondValueDF(B, LSC)
  LSC$Slope = OriginalSlope - B$ChangeInYTM
  DownBV = BondValueDF(B, LSC)
  LSC$Slope = OriginalSlope
  BCMDSlope = (DownBV - UpBV)/(2.0*OriginalBV*(B$ChangeInYTM/100.0))
  return( BCMDSlope ) 
}
#
# LSC Modified Duration -- Curve1
#
LSCMDCurvature1 = function(B, LSC){
  OriginalCurvature1 = LSC$Curvature1
  OriginalBV = BondValueDF(B, LSC)
  LSC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpBV = BondValueDF(B, LSC)
  LSC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownBV = BondValueDF(B, LSC)
  LSC$Curvature1 = OriginalCurvature1
  BCMDCurvature1 = (DownBV - UpBV)/(2.0*OriginalBV*(B$ChangeInYTM/100.0))
  return( BCMDCurvature1 ) 
}
#
# LSC Convexity -- Level
#
LSCCYLevel = function(B, LSC){
  OriginalLevel = LSC$Intercept
  OriginalBV = BondValueDF(B, LSC)
  LSC$Intercept = OriginalLevel + B$ChangeInYTM
  UpBV = BondValueDF(B, LSC)
  LSC$Intercept = OriginalLevel - B$ChangeInYTM
  DownBV = BondValueDF(B, LSC)
  LSC$Intercept = OriginalLevel
  TChange = (DownBV - OriginalBV) - (OriginalBV - UpBV)
  if(TChange > 0.00001){
    Num = log((DownBV - OriginalBV) - (OriginalBV - UpBV))
    Den = -log(OriginalBV) - 2.0*log(B$ChangeInYTM/100.0)
    BCCYLevel = exp(Num + Den)
  } else {
    BCCYLevel = 0.0
  }
  return( BCCYLevel ) 
}
#
# LSC Convexity -- Slope
#
LSCCYSlope = function(B, LSC){
  OriginalSlope = LSC$Slope
  OriginalBV = BondValueDF(B, LSC)
  LSC$Slope = OriginalSlope + B$ChangeInYTM
  UpBV = BondValueDF(B, LSC)
  LSC$Slope = OriginalSlope - B$ChangeInYTM
  DownBV = BondValueDF(B, LSC)
  LSC$Slope = OriginalSlope
  TChange = (DownBV - OriginalBV) - (OriginalBV - UpBV)
  if(TChange > 0.00001){
    Num = log((DownBV - OriginalBV) - (OriginalBV - UpBV))
    Den = -log(OriginalBV) - 2.0*log(B$ChangeInYTM/100.0)
    BCCYSlope = exp(Num + Den)
  } else {
    BCCYSlope = 0.0
  }
  return( BCCYSlope ) 
}
#
# LSC Convexity -- Curve1
#
LSCCYCurvature1 = function(B, LSC){
  OriginalCurvature1 = LSC$Curvature1
  OriginalBV = BondValueDF(B, LSC)
  LSC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpBV = BondValueDF(B, LSC)
  LSC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownBV = BondValueDF(B, LSC)
  LSC$Curvature1 = OriginalCurvature1
  TChange = (DownBV - OriginalBV) - (OriginalBV - UpBV)
  if(TChange > 0.00001){
    Num = log((DownBV - OriginalBV) - (OriginalBV - UpBV))
    Den = -log(OriginalBV) - 2.0*log(B$ChangeInYTM/100.0)
    BCCYCurvature1 = exp(Num + Den)
  } else {
    BCCYCurvature1 = 0.0
  }
  return( BCCYCurvature1 ) 
}
#
# LSC Cross Convexity -- Level and Slope
#
LSCCCLevelSlope = function(B, LSC){
  OriginalSlope = LSC$Slope
  LSC$Slope = OriginalSlope + B$ChangeInYTM
  UpV = LSCMDLevel(B, LSC)
  LSC$Slope = OriginalSlope - B$ChangeInYTM
  DownV = LSCMDLevel(B, LSC)
  LSC$Slope = OriginalSlope
  BCCCLevelSlope = (DownV - UpV)/(2.0*(B$ChangeInYTM/100.0))
  return( BCCCLevelSlope ) 
}
#
# LSC Cross Convexity -- Level and Curve1
#
LSCCCLevelCurvature1 = function(B, LSC){
  OriginalCurvature1 = LSC$Curvature1
  LSC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpV = LSCMDLevel(B, LSC)
  LSC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownV = LSCMDLevel(B, LSC)
  LSC$Curvature1 = OriginalCurvature1
  BCCCLevelCurvature1 = (DownV - UpV)/(2.0*(B$ChangeInYTM/100.0))
  return( BCCCLevelCurvature1 ) 
}
#
# LSC Cross Convexity -- Slope and Curve1
#
LSCCCSlopeCurvature1 = function(B, LSC){
  OriginalCurvature1 = LSC$Curvature1
  LSC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpV = LSCMDSlope(B, LSC)
  LSC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownV = LSCMDSlope(B, LSC)
  LSC$Curvature1 = OriginalCurvature1
  BCCCSlopeCurvature1 = (DownV - UpV)/(2.0*(B$ChangeInYTM/100.0))
  return( BCCCSlopeCurvature1 ) 
}
