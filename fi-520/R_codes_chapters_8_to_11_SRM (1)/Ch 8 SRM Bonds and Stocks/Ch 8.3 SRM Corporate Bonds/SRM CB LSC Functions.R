# SRM CB LSC Functions.R
#  Numerical derivatives
#
# Base Curve Modified Duration -- Level
#
BaseCurveMDLevel = function(B, BC, SC){
  OriginalLevel = BC$Intercept
  OriginalBV = BondValueDF2(B, BC, SC)
  BC$Intercept = OriginalLevel + B$ChangeInYTM
  UpBV = BondValueDF2(B, BC, SC)
  BC$Intercept = OriginalLevel - B$ChangeInYTM
  DownBV = BondValueDF2(B, BC, SC)
  BC$Intercept = OriginalLevel
  BCMDLevel = (DownBV - UpBV)/(2.0*OriginalBV*(B$ChangeInYTM/100.0))
  return( BCMDLevel ) 
}
#
# Spread Curve Modified Duration -- Level
#
SpreadCurveMDLevel = function(B, BC, SC){
  OriginalLevel = SC$Intercept
  OriginalBV = BondValueDF2(B, BC, SC)
  SC$Intercept = OriginalLevel + B$ChangeInYTM
  UpBV = BondValueDF2(B, BC, SC)
  SC$Intercept = OriginalLevel - B$ChangeInYTM
  DownBV = BondValueDF2(B, BC, SC)
  SC$Intercept = OriginalLevel
  SCMDLevel = (DownBV - UpBV)/(2.0*OriginalBV*(B$ChangeInYTM/100.0))
  return( SCMDLevel ) 
}
#
# Base Curve Modified Duration -- Slope
#
BaseCurveMDSlope = function(B, BC, SC){
  OriginalSlope = BC$Slope
  OriginalBV = BondValueDF2(B, BC, SC)
  BC$Slope = OriginalSlope + B$ChangeInYTM
  UpBV = BondValueDF2(B, BC, SC)
  BC$Slope = OriginalSlope - B$ChangeInYTM
  DownBV = BondValueDF2(B, BC, SC)
  BC$Slope = OriginalSlope
  BCMDSlope = (DownBV - UpBV)/(2.0*OriginalBV*(B$ChangeInYTM/100.0))
  return( BCMDSlope ) 
}
#
# Spread Curve Modified Duration -- Slope
#
SpreadCurveMDSlope = function(B, BC, SC){
  OriginalSlope = SC$Slope
  OriginalBV = BondValueDF2(B, BC, SC)
  SC$Slope = OriginalSlope + B$ChangeInYTM
  UpBV = BondValueDF2(B, BC, SC)
  SC$Slope = OriginalSlope - B$ChangeInYTM
  DownBV = BondValueDF2(B, BC, SC)
  SC$Slope = OriginalSlope
  SCMDSlope = (DownBV - UpBV)/(2.0*OriginalBV*(B$ChangeInYTM/100.0))
  return( SCMDSlope ) 
}
#
# Base Curve Modified Duration -- Curvature1
#
BaseCurveMDCurvature1 = function(B, BC, SC){
  OriginalCurvature1 = BC$Curvature1
  OriginalBV = BondValueDF2(B, BC, SC)
  BC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpBV = BondValueDF2(B, BC, SC)
  BC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownBV = BondValueDF2(B, BC, SC)
  BC$Curvature1 = OriginalCurvature1
  BCMDCurvature1 = (DownBV - UpBV)/(2.0*OriginalBV*(B$ChangeInYTM/100.0))
  return( BCMDCurvature1 ) 
}
#
# Spread Curve Modified Duration -- Curvature1
#
SpreadCurveMDCurvature1 = function(B, BC, SC){
  OriginalCurvature1 = SC$Curvature1
  OriginalBV = BondValueDF2(B, BC, SC)
  SC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpBV = BondValueDF2(B, BC, SC)
  SC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownBV = BondValueDF2(B, BC, SC)
  SC$Curvature1 = OriginalCurvature1
  SCMDCurvature1 = (DownBV - UpBV)/(2.0*OriginalBV*(B$ChangeInYTM/100.0))
  return( SCMDCurvature1 ) 
}
#
# Base Curve Convexity -- Level
#
BaseCurveCYLevel = function(B, BC, SC){
  OriginalLevel = BC$Intercept
  OriginalBV = BondValueDF2(B,  BC, SC)
  BC$Intercept = OriginalLevel + B$ChangeInYTM
  UpBV = BondValueDF2(B,  BC, SC)
  BC$Intercept = OriginalLevel - B$ChangeInYTM
  DownBV = BondValueDF2(B,  BC, SC)
  BC$Intercept = OriginalLevel
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
# Spread Curve Convexity -- Level
#
SpreadCurveCYLevel = function(B, BC, SC){
  OriginalLevel = SC$Intercept
  OriginalBV = BondValueDF2(B,  BC, SC)
  SC$Intercept = OriginalLevel + B$ChangeInYTM
  UpBV = BondValueDF2(B,  BC, SC)
  SC$Intercept = OriginalLevel - B$ChangeInYTM
  DownBV = BondValueDF2(B,  BC, SC)
  SC$Intercept = OriginalLevel
  TChange = (DownBV - OriginalBV) - (OriginalBV - UpBV)
  if(TChange > 0.00001){
    Num = log((DownBV - OriginalBV) - (OriginalBV - UpBV))
    Den = -log(OriginalBV) - 2.0*log(B$ChangeInYTM/100.0)
    SCCYLevel = exp(Num + Den)
  } else {
    SCCYLevel = 0.0
  }
  return( SCCYLevel )
}
#
# Base Curve Convexity -- Slope
#
BaseCurveCYSlope = function(B, BC, SC){
  OriginalSlope = BC$Slope
  OriginalBV = BondValueDF2(B,  BC, SC)
  BC$Slope = OriginalSlope + B$ChangeInYTM
  UpBV = BondValueDF2(B,  BC, SC)
  BC$Slope = OriginalSlope - B$ChangeInYTM
  DownBV = BondValueDF2(B,  BC, SC)
  BC$Slope = OriginalSlope
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
# Spread Curve Convexity -- Slope
#
SpreadCurveCYSlope = function(B, BC, SC){
  OriginalSlope = SC$Slope
  OriginalBV = BondValueDF2(B,  BC, SC)
  SC$Slope = OriginalSlope + B$ChangeInYTM
  UpBV = BondValueDF2(B,  BC, SC)
  SC$Slope = OriginalSlope - B$ChangeInYTM
  DownBV = BondValueDF2(B,  BC, SC)
  SC$Slope = OriginalSlope
  TChange = (DownBV - OriginalBV) - (OriginalBV - UpBV)
  if(TChange > 0.00001){
    Num = log((DownBV - OriginalBV) - (OriginalBV - UpBV))
    Den = -log(OriginalBV) - 2.0*log(B$ChangeInYTM/100.0)     # NOTE
    SCCYSlope = exp(Num + Den)
  } else {
    SCCYSlope = 0.0
  }
  return( SCCYSlope )
}
#
# Base Curve Convexity -- Curvature1
#
BaseCurveCYCurvature1 = function(B, BC, SC){
  OriginalCurvature1 = BC$Curvature1
  OriginalBV = BondValueDF2(B,  BC, SC)
  BC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpBV = BondValueDF2(B,  BC, SC)
  BC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownBV = BondValueDF2(B,  BC, SC)
  BC$Curvature1 = OriginalCurvature1
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
# Spread Curve Convexity -- Curvature1
#
SpreadCurveCYCurvature1 = function(B, BC, SC){
  OriginalCurvature1 = SC$Curvature1
  OriginalBV = BondValueDF2(B,  BC, SC)
  SC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpBV = BondValueDF2(B,  BC, SC)
  SC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownBV = BondValueDF2(B,  BC, SC)
  SC$Curvature1 = OriginalCurvature1
  TChange = (DownBV - OriginalBV) - (OriginalBV - UpBV)
  if(TChange > 0.00001){
    Num = log((DownBV - OriginalBV) - (OriginalBV - UpBV))
    Den = -log(OriginalBV) - 2.0*log(B$ChangeInYTM/100.0)     # NOTE
    SCCYCurvature1 = exp(Num + Den)
  } else {
    SCCYCurvature1 = 0.0
  }
  return( SCCYCurvature1 )
}
#
# Base Curve Cross Convexity -- Level and Slope
#
BaseCurveCCLevelSlope = function(B, BC, SC){
  OriginalSlope = BC$Slope
  BC$Slope = OriginalSlope + B$ChangeInYTM
  UpV = BaseCurveMDLevel(B,  BC, SC)
  BC$Slope = OriginalSlope - B$ChangeInYTM
  DownV = BaseCurveMDLevel(B,  BC, SC)
  BC$Slope = OriginalSlope
  BCCCLevelSlope = (DownV - UpV)/(2.0*(B$ChangeInYTM/100.0))
  return( BCCCLevelSlope )
}
#
# Spread Curve Cross Convexity -- Level and Slope
#
SpreadCurveCCLevelSlope = function(B, BC, SC){
  OriginalSlope = SC$Slope
  SC$Slope = OriginalSlope + B$ChangeInYTM
  UpV = BaseCurveMDLevel(B,  BC, SC)
  SC$Slope = OriginalSlope - B$ChangeInYTM
  DownV = BaseCurveMDLevel(B,  BC, SC)
  SC$Slope = OriginalSlope
  SCCCLevelSlope = (DownV - UpV)/(2.0*(B$ChangeInYTM/100.0))
  return( SCCCLevelSlope )
}
#
# Base Curve Cross Convexity -- Level and Curvature1
#
BaseCurveCCLevelCurvature1 = function(B, BC, SC){
  OriginalCurvature1 = BC$Curvature1
  BC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpV = BaseCurveMDLevel(B,  BC, SC)
  BC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownV = BaseCurveMDLevel(B,  BC, SC)
  BC$Curvature1 = OriginalCurvature1
  BCCCLevelCurvature1 = (DownV - UpV)/(2.0*(B$ChangeInYTM/100.0))
  return( BCCCLevelCurvature1 )
}
#
# Spread Curve Cross Convexity -- Level and Curvature1
#
SpreadCurveCCLevelCurvature1 = function(B, BC, SC){
  OriginalCurvature1 = SC$Curvature1
  SC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpV = BaseCurveMDLevel(B,  BC, SC)
  SC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownV = BaseCurveMDLevel(B,  BC, SC)
  SC$Curvature1 = OriginalCurvature1
  SCCCLevelCurvature1 = (DownV - UpV)/(2.0*(B$ChangeInYTM/100.0))
  return( SCCCLevelCurvature1 )
}
#
# Base Curve Cross Convexity -- Slope and Curvature1
#
BaseCurveCCSlopeCurvature1 = function(B, BC, SC){
  OriginalCurvature1 = BC$Curvature1
  BC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpV = BaseCurveMDSlope(B,  BC, SC)
  BC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownV = BaseCurveMDSlope(B,  BC, SC)
  BC$Curvature1 = OriginalCurvature1
  BCCCSlopeCurvature1 = (DownV - UpV)/(2.0*(B$ChangeInYTM/100.0))
  return( BCCCSlopeCurvature1 )
}
#
# Spread Curve Cross Convexity -- Slope and Curvature1
#
SpreadCurveCCSlopeCurvature1 = function(B, BC, SC){
  OriginalCurvature1 = SC$Curvature1
  SC$Curvature1 = OriginalCurvature1 + B$ChangeInYTM
  UpV = BaseCurveMDSlope(B,  BC, SC)
  SC$Curvature1 = OriginalCurvature1 - B$ChangeInYTM
  DownV = BaseCurveMDSlope(B,  BC, SC)
  SC$Curvature1 = OriginalCurvature1
  SCCCSlopeCurvature1 = (DownV - UpV)/(2.0*(B$ChangeInYTM/100.0))
  return( SCCCSlopeCurvature1 )
}
