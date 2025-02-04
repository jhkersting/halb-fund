# Core Functions Tests.R
#  Sub program for Module 9.3
#
# Core functions tests
#
options(digits = RDigits)
UIValue = ABMInputData$StockPrice
TestPV1 = PV1(inputTimeToMaturity, inputInterestRate)
Testdn = dn(ABMInputData)
Testn = n(dn(ABMInputData))
TestN = N(dn(ABMInputData))
cat(paste0("Underlying Value = ", round(UIValue,RDigits), 
  "\nPV = ", round(TestPV1,RDigits),
  "\ndn = ", round(Testdn,RDigits),
  "\nn(dn) = ", round(Testn,RDigits), "\nN(dn) = ", round(TestN,RDigits)))
UIValue; TestPV1; Testdn; Testn; TestN
#
# ABM call functions tests (boundaries, value, greeks, and implied parameters)
#
ABMInputData$Type = 1
CallLowerBound = OptionLowerBound(ABMInputData)
cat(paste0("\nCall Lower Bound = ", round(CallLowerBound,RDigits)))
CallLowerBound
CallUpperBound = OptionUpperBound(ABMInputData)
cat(paste0("\nCall Upper Bound = ", round(CallUpperBound,RDigits)))
CallUpperBound
CallABMValue = ABMOptionValue(ABMInputData)
cat(paste0("\nCall ABM Value = ", round(CallABMValue,RDigits)))
CallABMValue
CallABMDelta <- ABMOptionDelta(ABMInputData)
cat(paste0("\nCall ABM Delta = ", round(CallABMDelta,RDigits)))
CallABMDelta
CallABMGamma <- ABMOptionGamma(ABMInputData)
cat(paste0("\nCall ABM Gamma = ", round(CallABMGamma,RDigits)))
CallABMGamma
CallABMTheta <- ABMOptionTheta(ABMInputData)
cat(paste0("\nCall ABM Theta = ", round(CallABMTheta,RDigits)))
CallABMTheta
CallABMVega <- ABMOptionVega(ABMInputData)
cat(paste0("\nCall ABM Vega = ", round(CallABMVega,RDigits)))
CallABMVega
CallABMRho <- ABMOptionRho(ABMInputData)
cat(paste0("\nCall ABM Rho = ", round(CallABMRho,RDigits)))
CallABMRho
#
# ABM put functions tests (boundaries, value, greeks, and implied parameters)
#
ABMInputData$Type = -1
PutLowerBound = OptionLowerBound(ABMInputData)
cat(paste0("\nPut Lower Bound = ", round(PutLowerBound,RDigits)))
PutLowerBound
PutUpperBound = OptionUpperBound(ABMInputData)
cat(paste0("\nPut Upper Bound = ", round(PutUpperBound,RDigits)))
PutUpperBound
PutABMValue = ABMOptionValue(ABMInputData)
cat(paste0("\nPut ABM Value = ", round(PutABMValue,RDigits)))
PutABMValue
PutABMDelta <- ABMOptionDelta(ABMInputData)
cat(paste0("\nPut ABM Delta = ", round(PutABMDelta,RDigits)))
PutABMDelta
PutABMGamma <- ABMOptionGamma(ABMInputData)
cat(paste0("\nPut ABM Gamma = ", round(PutABMGamma,RDigits)))
PutABMGamma
PutABMVega <- ABMOptionVega(ABMInputData)
cat(paste0("\nPut ABM Vega = ", round(PutABMVega,RDigits)))
PutABMVega
PutABMTheta <- ABMOptionTheta(ABMInputData)
cat(paste0("\nPut ABM Theta = ", round(PutABMTheta,RDigits)))
PutABMTheta
PutABMRho <- ABMOptionRho(ABMInputData)
cat(paste0("\nPut ABM Rho = ", round(PutABMRho,RDigits)))
PutABMRho
#
# Binomial functions (European-style)
#
# Value
ESBINValue = ABMESBINOptionValue(BINInputData)
ESBINValue
cat(paste0("\nCall ES Bin Value = ", 
  round(ESBINValue$CallValue,RDigits)), "\n")
cat(paste0("\nPut ES Bin Value = ", 
  round(ESBINValue$PutValue,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Value = ", 
  round(ESBINValue$DigitalCallValue,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Value = ", 
  round(ESBINValue$DigitalPutValue,RDigits)), "\n")
# Delta
ESBINDelta = ABMESBINOptionDeltaDirect(BINInputData)
cat(paste0("\nCall ES Bin Delta = ", 
  round(ESBINDelta$ESCallDelta,RDigits)), "\n")
cat(paste0("\nPut ES Bin Delta = ", 
  round(ESBINDelta$ESPutDelta,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Delta = ", 
  round(ESBINDelta$ESDigitalCallDelta,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Delta = ", 
  round(ESBINDelta$ESDigitalPutDelta,RDigits)), "\n")
# Delta enhanced
ESBINDelta = ABMESBINOptionDeltaDirectEnh(BINInputData)
cat(paste0("\nCall ES Bin Delta (Enhanced) = ", 
  round(ESBINDelta$ESCallDelta,RDigits)), "\n")
cat(paste0("\nPut ES Bin Delta (Enhanced) = ", 
  round(ESBINDelta$ESPutDelta,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Delta (Enhanced) = ", 
  round(ESBINDelta$ESDigitalCallDelta,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Delta (Enhanced) = ", 
  round(ESBINDelta$ESDigitalPutDelta,RDigits)), "\n")
# Numerical delta
ESBINDelta = ABMESBINOptionNDelta(BINInputData)
cat(paste0("\nCall ES Bin Delta = ", 
  round(ESBINDelta$ESCallDelta,RDigits)), "\n")
cat(paste0("\nPut ES Bin Delta = ", 
  round(ESBINDelta$ESPutDelta,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Delta = ", 
  round(ESBINDelta$ESDigitalCallDelta,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Delta = ", 
  round(ESBINDelta$ESDigitalPutDelta,RDigits)), "\n")
# Gamma
ESBINGamma = ABMESBINOptionGammaDirect(BINInputData)
cat(paste0("\nCall ES Bin Gamma = ", 
  round(ESBINGamma$ESCallGamma,RDigits)), "\n")
cat(paste0("\nPut ES Bin Gamma = ", 
  round(ESBINGamma$ESPutGamma,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Gamma = ", 
  round(ESBINGamma$ESDigitalCallGamma,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Gamma = ", 
  round(ESBINGamma$ESDigitalPutGamma,RDigits)), "\n")
# Gamma enhanced
ESBINGamma = ABMESBINOptionGammaDirectEnh(BINInputData)
cat(paste0("\nCall ES Bin Gamma (Enhanced) = ", 
  round(ESBINGamma$ESCallGamma,RDigits)), "\n")
cat(paste0("\nPut ES Bin Gamma (Enhanced) = ", 
  round(ESBINGamma$ESPutGamma,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Gamma (Enhanced) = ", 
  round(ESBINGamma$ESDigitalCallGamma,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Gamma (Enhanced) = ", 
  round(ESBINGamma$ESDigitalPutGamma,RDigits)), "\n")
# Numerical gamma
ESBINGamma = ABMESBINOptionNGamma(BINInputData)
cat(paste0("\nCall ES Bin Gamma = ", 
  round(ESBINGamma$ESCallGamma,RDigits)), "\n")
cat(paste0("\nPut ES Bin Gamma = ", 
  round(ESBINGamma$ESPutGamma,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Gamma = ", 
  round(ESBINGamma$ESDigitalCallGamma,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Gamma = ", 
  round(ESBINGamma$ESDigitalPutGamma,RDigits)), "\n")
# Theta
ESBINTheta = ABMESBINOptionThetaDirect(BINInputData)
cat(paste0("\nCall ES Bin Theta = ", 
  round(ESBINTheta$ESCallTheta,RDigits)), "\n")
cat(paste0("\nPut ES Bin Theta = ", 
  round(ESBINTheta$ESPutTheta,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Theta = ", 
  round(ESBINTheta$ESDigitalCallTheta,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Theta = ", 
  round(ESBINTheta$ESDigitalPutTheta,RDigits)), "\n")
# Theta
ESBINTheta = ABMESBINOptionThetaDirectEnh(BINInputData)
cat(paste0("\nCall ES Bin Theta (Enhanced) = ", 
  round(ESBINTheta$ESCallTheta,RDigits)), "\n")
cat(paste0("\nPut ES Bin Theta (Enhanced) = ", 
  round(ESBINTheta$ESPutTheta,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Theta (Enhanced) = ", 
  round(ESBINTheta$ESDigitalCallTheta,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Theta (Enhanced) = ", 
  round(ESBINTheta$ESDigitalPutTheta,RDigits)), "\n")
# Numerical theta
ESBINTheta = ABMESBINOptionNTheta(BINInputData)
cat(paste0("\nCall ES Bin Theta = ", 
  round(ESBINTheta$ESCallTheta,RDigits)), "\n")
cat(paste0("\nPut ES Bin Theta = ", 
  round(ESBINTheta$ESPutTheta,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Theta = ", 
  round(ESBINTheta$ESDigitalCallTheta,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Theta = ", 
  round(ESBINTheta$ESDigitalPutTheta,RDigits)), "\n")
# Vega
ESBINVega = ABMESBINOptionNVega(BINInputData)
cat(paste0("\nCall ES Bin Vega = ", 
  round(ESBINVega$ESCallVega,RDigits)), "\n")
cat(paste0("\nPut ES Bin Vega = ", 
  round(ESBINVega$ESPutVega,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Vega = ", 
  round(ESBINVega$ESDigitalCallVega,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Vega = ", 
  round(ESBINVega$ESDigitalPutVega,RDigits)), "\n")
# Rho
ESBINRho = ABMESBINOptionNRho(BINInputData)
cat(paste0("\nCall ES Bin Rho = ", 
  round(ESBINRho$ESCallRho,RDigits)), "\n")
cat(paste0("\nPut ES Bin Rho = ", 
  round(ESBINRho$ESPutRho,RDigits)), "\n")
cat(paste0("\nCall Digital ES Bin Rho = ", 
  round(ESBINRho$ESDigitalCallRho,RDigits)), "\n")
cat(paste0("\nPut Digital ES Bin Rho = ", 
  round(ESBINRho$ESDigitalPutRho,RDigits)), "\n")
#
# Binomial functions (American-style)
#
# Value
ASBINValue = ABMASOptionValue(BINInputData)
cat(paste0("\nCall AS Bin Value = ", 
  round(ASBINValue$CallValue,RDigits)), "\n")
cat(paste0("\nPut AS Bin Value = ", 
  round(ASBINValue$PutValue,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Value = ", 
  round(ASBINValue$DigitalCallValue,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Value = ", 
  round(ASBINValue$DigitalPutValue,RDigits)), "\n")
# Delta
ASBINDelta = ABMASBINOptionDeltaDirect(BINInputData)
cat(paste0("\nCall AS Bin Delta = ", 
  round(ASBINDelta$ASCallDelta,RDigits)), "\n")
cat(paste0("\nPut AS Bin Delta = ", 
  round(ASBINDelta$ASPutDelta,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Delta = ", 
  round(ASBINDelta$ASDigitalCallDelta,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Delta = ", 
  round(ASBINDelta$ASDigitalPutDelta,RDigits)), "\n")
# Delta enhanced
ASBINDelta = ABMASBINOptionDeltaDirectEnh(BINInputData)
cat(paste0("\nCall AS Bin Delta (Enhanced) = ", 
  round(ASBINDelta$ASCallDelta,RDigits)), "\n")
cat(paste0("\nPut AS Bin Delta (Enhanced) = ", 
  round(ASBINDelta$ASPutDelta,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Delta (Enhanced) = ", 
  round(ASBINDelta$ASDigitalCallDelta,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Delta (Enhanced) = ", 
  round(ASBINDelta$ASDigitalPutDelta,RDigits)), "\n")
# Numerical delta
ASBINDelta = ABMASBINOptionNDelta(BINInputData)
cat(paste0("\nCall AS Bin Delta = ", 
  round(ASBINDelta$ASCallDelta,RDigits)), "\n")
cat(paste0("\nPut AS Bin Delta = ", 
  round(ASBINDelta$ASPutDelta,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Delta = ", 
  round(ASBINDelta$ASDigitalCallDelta,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Delta = ", 
  round(ASBINDelta$ASDigitalPutDelta,RDigits)), "\n")
# Gamma
ASBINGamma = ABMASBINOptionGammaDirect(BINInputData)
cat(paste0("\nCall AS Bin Gamma = ", 
  round(ASBINGamma$ASCallGamma,RDigits)), "\n")
cat(paste0("\nPut AS Bin Gamma = ", 
  round(ASBINGamma$ASPutGamma,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Gamma = ", 
  round(ASBINGamma$ASDigitalCallGamma,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Gamma = ", 
  round(ASBINGamma$ASDigitalPutGamma,RDigits)), "\n")
# Gamma enhanced
ASBINGamma = ABMASBINOptionGammaDirectEnh(BINInputData)
cat(paste0("\nCall AS Bin Gamma (Enhanced) = ", 
  round(ASBINGamma$ASCallGamma,RDigits)), "\n")
cat(paste0("\nPut AS Bin Gamma (Enhanced) = ", 
  round(ASBINGamma$ASPutGamma,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Gamma (Enhanced) = ", 
  round(ASBINGamma$ASDigitalCallGamma,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Gamma (Enhanced) = ", 
  round(ASBINGamma$ASDigitalPutGamma,RDigits)), "\n")
# Numerical gamma
ASBINGamma = ABMASBINOptionNGamma(BINInputData)
cat(paste0("\nCall AS Bin Gamma = ", 
  round(ASBINGamma$ASCallGamma,RDigits)), "\n")
cat(paste0("\nPut AS Bin Gamma = ", 
  round(ASBINGamma$ASPutGamma,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Gamma = ", 
  round(ASBINGamma$ASDigitalCallGamma,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Gamma = ", 
  round(ASBINGamma$ASDigitalPutGamma,RDigits)), "\n")
# Theta
ASBINTheta = ABMASBINOptionThetaDirect(BINInputData)
cat(paste0("\nCall AS Bin Theta = ", 
  round(ASBINTheta$ASCallTheta,RDigits)), "\n")
cat(paste0("\nPut AS Bin Theta = ", 
  round(ASBINTheta$ASPutTheta,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Theta = ", 
  round(ASBINTheta$ASDigitalCallTheta,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Theta = ", 
  round(ASBINTheta$ASDigitalPutTheta,RDigits)), "\n")
# Theta
ASBINTheta = ABMASBINOptionThetaDirectEnh(BINInputData)
cat(paste0("\nCall AS Bin Theta (Enhanced) = ", 
  round(ASBINTheta$ASCallTheta,RDigits)), "\n")
cat(paste0("\nPut AS Bin Theta (Enhanced) = ", 
  round(ASBINTheta$ASPutTheta,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Theta (Enhanced) = ", 
  round(ASBINTheta$ASDigitalCallTheta,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Theta (Enhanced) = ", 
  round(ASBINTheta$ASDigitalPutTheta,RDigits)), "\n")
# Numerical theta
ASBINTheta = ABMASBINOptionNTheta(BINInputData)
cat(paste0("\nCall AS Bin Theta = ", 
  round(ASBINTheta$ASCallTheta,RDigits)), "\n")
cat(paste0("\nPut AS Bin Theta = ", 
  round(ASBINTheta$ASPutTheta,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Theta = ", 
  round(ASBINTheta$ASDigitalCallTheta,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Theta = ", 
  round(ASBINTheta$ASDigitalPutTheta,RDigits)), "\n")
# Vega
ASBINVega = ABMASBINOptionNVega(BINInputData)
cat(paste0("\nCall AS Bin Vega = ", 
  round(ASBINVega$ASCallVega,RDigits)), "\n")
cat(paste0("\nPut AS Bin Vega = ", 
  round(ASBINVega$ASPutVega,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Vega = ", 
  round(ASBINVega$ASDigitalCallVega,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Vega = ", 
  round(ASBINVega$ASDigitalPutVega,RDigits)), "\n")
# Rho
ASBINRho = ABMASBINOptionNRho(BINInputData)
cat(paste0("\nCall AS Bin Rho = ", 
  round(ASBINRho$ASCallRho,RDigits)), "\n")
cat(paste0("\nPut AS Bin Rho = ", 
  round(ASBINRho$ASPutRho,RDigits)), "\n")
cat(paste0("\nCall Digital AS Bin Rho = ", 
  round(ASBINRho$ASDigitalCallRho,RDigits)), "\n")
cat(paste0("\nPut Digital AS Bin Rho = ", 
  round(ASBINRho$ASDigitalPutRho,RDigits)), "\n")
