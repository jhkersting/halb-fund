# SRM SWAP Factor Duration and Convexity Functions.R
#
# Factor duration 
#
# FD Forward Curve Level
FDFCL = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  FD <- FDwrtFCL(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*FD/(CC + SVttd - SVtt))
}
# FD  Forward Curve Slope
FDFCS = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  FD <- FDwrtFCS(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*FD/(CC + SVttd - SVtt))
}
# FD  Forward Curve Curvature 1
FDFCC1 = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  FD <- FDwrtFCC1(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*FD/(CC + SVttd - SVtt))
}
# FD Basis Curve Level
FDBCL = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  FD <- FDwrtBCL(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*FD/(CC + SVttd - SVtt))
}
# FD  Basis Curve Slope
FDBCS = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  FD <- FDwrtBCS(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*FD/(CC + SVttd - SVtt))
}
# FD  Basis Curve Curvature 1
FDBCC1 = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  FD <- FDwrtBCC1(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*FD/(CC + SVttd - SVtt))
}
#
# Factor convexities 
#
# FCx Forward Curve Level
FCxFCL = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtFCL(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}
# FCx  Forward Curve Slope
FCxFCS = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtFCS(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}
# FCx  Forward Curve Curvature 1
FCxFCC1 = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtFCC1(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}
# FCx Basis Curve Level
FCxBCL = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtBCL(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}
# FCx  Basis Curve Slope
FCxBCS = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtBCS(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}
# FCx  Basis Curve Curvature 1
FCxBCC1 = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtBCC1(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}
#
# Factor cross convexities 
#
# FCC Forward Curve Level and Slope
FCCFCLS = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtFCLS(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}
# FCC  Forward Curve Slope and Curvature 1
FCCFCSC1 = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtFCSC1(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}
# FCC  Forward Curve Level and Curvature 1
FCCFCLC1 = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtFCLC1(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}
# FCC Basis Curve Level and Slope
FCCBCLS = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtBCLS(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}
# FCC  Basis Curve Slope and Curvature 1
FCCBCSC1 = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtBCSC1(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}
# FCC  Basis Curve Level and Curvature 1
FCCBCLC1 = function(dfSWAP, dfHDInputs){
  SVttd <- SwapValuettd(dfSWAP, dfHDInputs)
  SVtt <- SwapValuett(dfSWAP)
  SD <- SDwrtBCLC1(dfSWAP, dfHDInputs)
  CC <- (dfSWAP$CashCollateral/100)*dfSWAP$NAmt
  return(-100*SD/(CC + SVttd - SVtt))
}

