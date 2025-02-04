# Core Functions Tests.R
#  Sub program for Module 9.3
#
# Core functions tests
#
options(digits = RDigits)
UIValue = GBMInputData$StockPrice
TestPV1 = PV1(inputTimeToMaturity, inputInterestRate)
Testd1 = d1(GBMInputData)
Testd2 = d2(GBMInputData)
Testn = n(d1(GBMInputData))
TestN = N(d1(GBMInputData))
cat(paste0("Underlying Value = ", round(UIValue,RDigits), 
  "\nPV = ", round(TestPV1,RDigits),
  "\nd1 = ", round(Testd1,RDigits), "\nd2 = ", round(Testd2,RDigits), 
  "\nn(d1) = ", round(Testn,RDigits), "\nN(d1) = ", round(TestN,RDigits)))
UIValue; TestPV1; Testd1; Testd2; Testn; TestN
#
# GBM call functions tests (boundaries, value, greeks, and implied parameters)
#
GBMInputData$Type = 1
CallLowerBound = OptionLowerBound(GBMInputData)
cat(paste0("\nCall Lower Bound = ", round(CallLowerBound,RDigits)))
CallLowerBound
CallUpperBound = OptionUpperBound(GBMInputData)
cat(paste0("\nCall Upper Bound = ", round(CallUpperBound,RDigits)))
CallUpperBound
CallGBMValue = GBMOptionValue(GBMInputData)
cat(paste0("\nCall GBM Value = ", round(CallGBMValue,RDigits)))
CallGBMValue
CallGBMDelta <- GBMOptionDelta(GBMInputData)
cat(paste0("\nCall GBM Delta = ", round(CallGBMDelta,RDigits)))
CallGBMDelta
CallGBMGamma <- GBMOptionGamma(GBMInputData)
cat(paste0("\nCall GBM Gamma = ", round(CallGBMGamma,RDigits)))
CallGBMGamma
CallGBMVega <- GBMOptionVega(GBMInputData)
cat(paste0("\nCall GBM Vega = ", round(CallGBMVega,RDigits)))
CallGBMVega
CallGBMTheta <- GBMOptionTheta(GBMInputData)
cat(paste0("\nCall GBM Theta = ", round(CallGBMTheta,RDigits)))
CallGBMTheta
CallGBMRho <- GBMOptionRho(GBMInputData)
cat(paste0("\nCall GBM Rho = ", round(CallGBMRho,RDigits)))
CallGBMRho


# inputOptionValue = CallGBMValue  # CallValue from GBMOptionValue function
# ImpliedCallGBMVolatility = 
#   GBMDYOptionImpliedVolatility(GBMInputData, inputOptionValue)
# cat(paste0("\nCall GBM Implied Volatility = ", round(ImpliedCallGBMVolatility,RDigits)))
# ImpliedCallGBMVolatility
# ImpliedCallGBMStockPrice = 
#   GBMDYOptionImpliedStockPrice(GBMInputData, inputOptionValue)
# cat(paste0("\nCall GBM Implied Stock Price = ", round(ImpliedCallGBMStockPrice,RDigits)))
# ImpliedCallGBMStockPrice
# ImpliedCallGBMStrikePrice = 
#   GBMDYOptionImpliedStrikePrice(GBMInputData, inputOptionValue)
# cat(paste0("\nCall GBM Implied Strike Price = ", round(ImpliedCallGBMStrikePrice,RDigits)))
# ImpliedCallGBMStrikePrice
# ImpliedCallGBMTimeToMaturity = 
#   GBMDYOptionImpliedTimeToMaturity(GBMInputData, inputOptionValue)
# cat(paste0("\nCall GBM Implied Time To Maturity = ", round(ImpliedCallGBMTimeToMaturity,RDigits)))
# ImpliedCallGBMTimeToMaturity
# ImpliedCallGBMInterestRate = 
#   GBMDYOptionImpliedInterestRate(GBMInputData, inputOptionValue)
# cat(paste0("\nCall GBM Implied Interest Rate = ", round(ImpliedCallGBMInterestRate,RDigits)))
# ImpliedCallGBMInterestRate
# ImpliedCallGBMDividendYield = 
#   GBMDYOptionImpliedDividendYield(GBMInputData, inputOptionValue)
# cat(paste0("\nCall GBM Implied Dividend Yield = ", round(ImpliedCallGBMDividendYield,RDigits)))
# ImpliedCallGBMDividendYield


#
# GBM put functions tests (boundaries, value, greeks, and implied parameters)
#
GBMInputData$Type = -1
PutLowerBound = OptionLowerBound(GBMInputData)
cat(paste0("\nPut Lower Bound = ", round(PutLowerBound,RDigits)))
PutLowerBound
PutUpperBound = OptionUpperBound(GBMInputData)
cat(paste0("\nPut Upper Bound = ", round(PutUpperBound,RDigits)))
PutUpperBound
PutGBMValue = GBMOptionValue(GBMInputData)
cat(paste0("\nPut GBM Value = ", round(PutGBMValue,RDigits)))
PutGBMValue
PutGBMDelta <- GBMOptionDelta(GBMInputData)
cat(paste0("\nPut GBM Delta = ", round(PutGBMDelta,RDigits)))
PutGBMDelta
PutGBMGamma <- GBMOptionGamma(GBMInputData)
cat(paste0("\nPut GBM Gamma = ", round(PutGBMGamma,RDigits)))
PutGBMGamma
PutGBMVega <- GBMOptionVega(GBMInputData)
cat(paste0("\nPut GBM Vega = ", round(PutGBMVega,RDigits)))
PutGBMVega
PutGBMTheta <- GBMOptionTheta(GBMInputData)
cat(paste0("\nPut GBM Theta = ", round(PutGBMTheta,RDigits)))
PutGBMTheta
PutGBMRho <- GBMOptionRho(GBMInputData)
cat(paste0("\nPut GBM Rho = ", round(PutGBMRho,RDigits)))
PutGBMRho


# inputOptionValue = PutGBMValue  # PutValue from GBMOptionValue function
# ImpliedPutGBMVolatility = 
#   GBMDYOptionImpliedVolatility(GBMInputData, inputOptionValue)
# cat(paste0("\nPut GBM Implied Volatility = ", round(ImpliedPutGBMVolatility,RDigits)))
# ImpliedPutGBMVolatility
# ImpliedPutGBMStockPrice = 
#   GBMDYOptionImpliedStockPrice(GBMInputData, inputOptionValue)
# cat(paste0("\nPut GBM Implied Stock Price = ", round(ImpliedPutGBMStockPrice,RDigits)))
# ImpliedPutGBMStockPrice
# ImpliedPutGBMStrikePrice = 
#   GBMDYOptionImpliedStrikePrice(GBMInputData, inputOptionValue)
# cat(paste0("\nPut GBM Implied Strike Price = ", round(ImpliedPutGBMStrikePrice,RDigits)))
# ImpliedPutGBMStrikePrice
# ImpliedPutGBMTimeToMaturity = 
#   GBMDYOptionImpliedTimeToMaturity(GBMInputData, inputOptionValue)
# cat(paste0("\nPut GBM Implied Time To Maturity = ", round(ImpliedPutGBMTimeToMaturity,RDigits)))
# ImpliedPutGBMTimeToMaturity
# ImpliedPutGBMInterestRate = 
#   GBMDYOptionImpliedInterestRate(GBMInputData, inputOptionValue)
# cat(paste0("\nPut GBM Implied Interest Rate = ", round(ImpliedPutGBMInterestRate,RDigits)))
# ImpliedPutGBMInterestRate
# ImpliedPutGBMDividendYield = 
#   GBMDYOptionImpliedDividendYield(GBMInputData, inputOptionValue)
# cat(paste0("\nPut GBM Implied Dividend Yield = ", round(ImpliedPutGBMDividendYield,RDigits)))
# ImpliedPutGBMDividendYield


#
# Binomial functions (European-style)
#
BINInputData$Type = 1
CallESBINValue = ESBINOptionValue(BINInputData)
cat(paste0("\nCall ES Bin Value = ", round(CallESBINValue,RDigits)))
CallESBINValue
CallESBINDelta = ESBINOptionDelta(BINInputData)
cat(paste0("\nCall ES Bin Delta = ", round(CallESBINDelta,RDigits)))
CallESBINDelta
CallESBINDeltaDirect = ESBINOptionDeltaDirect(BINInputData)
cat(paste0("\nCall ES Bin Delta Direct = ", round(CallESBINDeltaDirect,RDigits)))
CallESBINDeltaDirect


CallESBINGamma = ESBINOptionGamma(BINInputData)
cat(paste0("\nCall ES Bin Gamma = ", round(CallESBINGamma,RDigits)))
CallESBINGamma


CallESBINGammaDirect = ESBINOptionGammaDirect(BINInputData)
cat(paste0("\nCall ES Bin Gamma Direct = ", round(CallESBINGammaDirect,RDigits)))
CallESBINGammaDirect
CallESBINTheta = ESBINOptionTheta(BINInputData)
cat(paste0("\nCall ES Bin Theta = ", round(CallESBINTheta,RDigits)))
CallESBINTheta
CallESBINVega = ESBINOptionVega(BINInputData)
cat(paste0("\nCall ES Bin Vega = ", round(CallESBINVega,RDigits)))
CallESBINVega
CallESBINRho = ESBINOptionRho(BINInputData)
cat(paste0("\nCall ES Bin Rho = ", round(CallESBINRho,RDigits)))
CallESBINRho


# # Implied parameters
# inputOptionValue = CallESBINValue  # CallValue from BINOptionValue function
# ImpliedCallESBINVolatility = 
#   ESBINOptionImpliedVolatility(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall ES Bin Implied Volatility = ", round(ImpliedCallESBINVolatility,RDigits)))
# ImpliedCallESBINVolatility
# ImpliedCallESBINStockPrice = 
#   ESBINOptionImpliedStockPrice(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall ES Bin Implied Stock Price = ", round(ImpliedCallESBINStockPrice,RDigits)))
# ImpliedCallESBINStockPrice
# ImpliedCallESBINStrikePrice = 
#   ESBINOptionImpliedStrikePrice(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall ES Bin Implied Strike Price = ", round(ImpliedCallESBINStrikePrice,RDigits)))
# ImpliedCallESBINStrikePrice
# ImpliedCallESBINTimeToMaturity = 
#   ESBINOptionImpliedTimeToMaturity(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall ES Bin Implied Time To Maturity = ", round(ImpliedCallESBINTimeToMaturity,RDigits)))
# ImpliedCallESBINTimeToMaturity
# ImpliedCallESBINInterestRate = 
#   ESBINOptionImpliedInterestRate(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall ES Bin Implied Interest Rate = ", round(ImpliedCallESBINInterestRate,RDigits)))
# ImpliedCallESBINInterestRate
# ImpliedCallESBINDividendYield = 
#   ESBINOptionImpliedDividendYield(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall ES Bin Implied Dividend Yield = ", round(ImpliedCallESBINDividendYield,RDigits)))
# ImpliedCallESBINDividendYield


# Puts
BINInputData$Type = -1
PutESBINValue = ESBINOptionValue(BINInputData)
cat(paste0("\nPut ES Bin Value = ", round(PutESBINValue,RDigits)))
PutESBINValue
PutESBINDelta = ESBINOptionDelta(BINInputData)
cat(paste0("\nPut ES Bin Delta = ", round(PutESBINDelta,RDigits)))
PutESBINDelta
PutESBINDeltaDirect = ESBINOptionDeltaDirect(BINInputData)
cat(paste0("\nPut ES Bin Delta Direct = ", round(PutESBINDeltaDirect,RDigits)))
PutESBINDeltaDirect
PutESBINGamma = ESBINOptionGamma(BINInputData)
cat(paste0("\nPut ES Bin Gamma = ", round(PutESBINGamma,RDigits)))
PutESBINGamma
PutESBINGammaDirect = ESBINOptionGammaDirect(BINInputData)
cat(paste0("\nPut ES Bin Gamma Direct = ", round(PutESBINGammaDirect,RDigits)))
PutESBINGammaDirect
PutESBINTheta = ESBINOptionTheta(BINInputData)
cat(paste0("\nPut ES Bin Theta = ", round(PutESBINTheta,RDigits)))
PutESBINTheta
PutESBINVega = ESBINOptionVega(BINInputData)
cat(paste0("\nPut ES Bin Vega = ", round(PutESBINVega,RDigits)))
PutESBINVega
PutESBINRho = ESBINOptionRho(BINInputData)
cat(paste0("\nPut ES Bin Rho = ", round(PutESBINRho,RDigits)))
PutESBINRho


# # Implied parameters
# inputOptionValue = PutESBINValue  # CallValue from BINOptionValue function
# ImpliedPutESBINVolatility = 
#   ESBINOptionImpliedVolatility(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut ES Bin Implied Volatility = ", round(ImpliedPutESBINVolatility,RDigits)))
# ImpliedPutESBINVolatility
# ImpliedPutESBINStockPrice = 
#   ESBINOptionImpliedStockPrice(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut ES Bin Implied Stock Price = ", round(ImpliedPutESBINStockPrice,RDigits)))
# ImpliedPutESBINStockPrice
# ImpliedPutESBINStrikePrice = 
#   ESBINOptionImpliedStrikePrice(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut ES Bin Implied Strike Price = ", round(ImpliedPutESBINStrikePrice,RDigits)))
# ImpliedPutESBINStrikePrice
# ImpliedPutESBINTimeToMaturity = 
#   ESBINOptionImpliedTimeToMaturity(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut ES Bin Implied Time To Maturity = ", round(ImpliedPutESBINTimeToMaturity,RDigits)))
# ImpliedPutESBINTimeToMaturity
# ImpliedPutESBINInterestRate = 
#   ESBINOptionImpliedInterestRate(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut ES Bin Implied Interest Rate = ", round(ImpliedPutESBINInterestRate,RDigits)))
# ImpliedPutESBINInterestRate
# ImpliedPutESBINDividendYield = 
#   ESBINOptionImpliedDividendYield(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut ES Bin Implied Dividend Yield = ", round(ImpliedPutESBINDividendYield,RDigits)))
# ImpliedPutESBINDividendYield


# #
# # Binomial functions (American-style)
# #
# BINInputData$Type = 1
# CallASBINValue = ASBINOptionValue(BINInputData)
# cat(paste0("\nCall AS Bin Value = ", round(CallASBINValue,RDigits)))
# CallASBINValue
# CallASBINDelta = ASBINOptionDelta(BINInputData)
# cat(paste0("\nCall AS Bin Delta = ", round(CallASBINDelta,RDigits)))
# CallASBINDelta
# CallASBINDeltaDirect = ASBINOptionDeltaDirect(BINInputData)
# cat(paste0("\nCall AS Bin Delta Direct = ", round(CallASBINDeltaDirect,RDigits)))
# CallASBINDeltaDirect
# CallASBINGamma = ASBINOptionGamma(BINInputData)
# cat(paste0("\nCall AS Bin Gamma = ", round(CallASBINGamma,RDigits)))
# CallASBINGamma
# CallASBINGammaDirect = ASBINOptionGammaDirect(BINInputData)
# cat(paste0("\nCall AS Bin Gamma Direct = ", round(CallASBINGammaDirect,RDigits)))
# CallASBINGammaDirect
# CallASBINTheta = ASBINOptionTheta(BINInputData)
# cat(paste0("\nCall AS Bin Theta = ", round(CallASBINTheta,RDigits)))
# CallASBINTheta
# CallASBINVega = ASBINOptionVega(BINInputData)
# cat(paste0("\nCall AS Bin Vega = ", round(CallASBINVega,RDigits)))
# CallASBINVega
# CallASBINRho = ASBINOptionRho(BINInputData)
# cat(paste0("\nCall AS Bin Rho = ", round(CallASBINRho,RDigits)))
# CallASBINRho
# # Implied parameters
# inputOptionValue = CallASBINValue  # CallValue from BINOptionValue function
# ImpliedCallASBINVolatility = 
#   ASBINOptionImpliedVolatility(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall AS Bin Implied Volatility = ", round(ImpliedCallASBINVolatility,RDigits)))
# ImpliedCallASBINVolatility
# ImpliedCallASBINStockPrice = 
#   ASBINOptionImpliedStockPrice(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall AS Bin Implied Stock Price = ", round(ImpliedCallASBINStockPrice,RDigits)))
# ImpliedCallASBINStockPrice
# ImpliedCallASBINStrikePrice = 
#   ASBINOptionImpliedStrikePrice(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall AS Bin Implied Strike Price = ", round(ImpliedCallASBINStrikePrice,RDigits)))
# ImpliedCallASBINStrikePrice
# ImpliedCallASBINTimeToMaturity = 
#   ASBINOptionImpliedTimeToMaturity(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall AS Bin Implied Time To Maturity = ", round(ImpliedCallASBINTimeToMaturity,RDigits)))
# ImpliedCallASBINTimeToMaturity
# ImpliedCallASBINInterestRate = 
#   ASBINOptionImpliedInterestRate(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall AS Bin Implied Interest Rate = ", round(ImpliedCallASBINInterestRate,RDigits)))
# ImpliedCallASBINInterestRate
# ImpliedCallASBINDividendYield = 
#   ASBINOptionImpliedDividendYield(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nCall AS Bin Implied Dividend Yield = ", round(ImpliedCallASBINDividendYield,RDigits)))
# ImpliedCallASBINDividendYield
# # Puts
# BINInputData$Type = -1
# PutASBINValue = ASBINOptionValue(BINInputData)
# cat(paste0("\nPut AS Bin Value = ", round(PutASBINValue,RDigits)))
# PutASBINValue
# PutASBINDelta = ASBINOptionDelta(BINInputData)
# cat(paste0("\nPut AS Bin Delta = ", round(PutASBINDelta,RDigits)))
# PutASBINDelta
# PutASBINDeltaDirect = ASBINOptionDeltaDirect(BINInputData)
# cat(paste0("\nPut AS Bin Delta Direct = ", round(PutASBINDeltaDirect,RDigits)))
# PutASBINDeltaDirect
# PutASBINGamma = ASBINOptionGamma(BINInputData)
# cat(paste0("\nPut AS Bin Gamma = ", round(PutASBINGamma,RDigits)))
# PutASBINGamma
# PutASBINGammaDirect = ASBINOptionGammaDirect(BINInputData)
# cat(paste0("\nPut AS Bin Gamma Direct = ", round(PutASBINGammaDirect,RDigits)))
# PutASBINGammaDirect
# PutASBINTheta = ASBINOptionTheta(BINInputData)
# cat(paste0("\nPut AS Bin Theta = ", round(PutASBINTheta,RDigits)))
# PutASBINTheta
# PutASBINVega = ASBINOptionVega(BINInputData)
# cat(paste0("\nPut AS Bin Vega = ", round(PutASBINVega,RDigits)))
# PutASBINVega
# PutASBINRho = ASBINOptionRho(BINInputData)
# cat(paste0("\nPut AS Bin Rho = ", round(PutASBINRho,RDigits)))
# PutASBINRho
# # Implied parameters
# inputOptionValue = PutASBINValue  # CallValue from BINOptionValue function
# ImpliedPutASBINVolatility = 
#   ASBINOptionImpliedVolatility(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut AS Bin Implied Volatility = ", round(ImpliedPutASBINVolatility,RDigits)))
# ImpliedPutASBINVolatility
# ImpliedPutASBINStockPrice = 
#   ASBINOptionImpliedStockPrice(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut AS Bin Implied Stock Price = ", round(ImpliedPutASBINStockPrice,RDigits)))
# ImpliedPutASBINStockPrice
# ImpliedPutASBINStrikePrice = 
#   ASBINOptionImpliedStrikePrice(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut AS Bin Implied Strike Price = ", round(ImpliedPutASBINStrikePrice,RDigits)))
# ImpliedPutASBINStrikePrice
# ImpliedPutASBINTimeToMaturity = 
#   ASBINOptionImpliedTimeToMaturity(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut AS Bin Implied Time To Maturity = ", round(ImpliedPutASBINTimeToMaturity,RDigits)))
# ImpliedPutASBINTimeToMaturity
# ImpliedPutASBINInterestRate = 
#   ASBINOptionImpliedInterestRate(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut AS Bin Implied Interest Rate = ", round(ImpliedPutASBINInterestRate,RDigits)))
# ImpliedPutASBINInterestRate
# ImpliedPutASBINDividendYield = 
#   ASBINOptionImpliedDividendYield(BINInputData, DIVInputData, inputOptionValue)
# cat(paste0("\nPut AS Bin Implied Dividend Yield = ", round(ImpliedPutASBINDividendYield,RDigits)))
# ImpliedPutASBINDividendYield
