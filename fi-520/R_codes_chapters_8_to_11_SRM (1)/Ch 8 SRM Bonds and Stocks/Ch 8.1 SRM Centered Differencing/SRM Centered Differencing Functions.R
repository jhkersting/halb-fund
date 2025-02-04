# SRM Centered Differencing Functions.R
#
source('SRM UST Functions.R')
#
# First Derivatives of Bond Value with respect to Yield to Maturity
#
BondFD = function(B){
  if (B$Order == 1){
    OriginalYTM = B$YieldToMaturity
    High = OriginalYTM + B$ChangeInYTM
    B$YieldToMaturity = High
    UpBV = BondValue(B)
    Low = OriginalYTM - B$ChangeInYTM
    B$YieldToMaturity = Low
    DownBV = BondValue(B)
    B$YieldToMaturity = OriginalYTM
    BFD = (UpBV - DownBV) / (High - Low)
    return( BFD ) 
  } else if (B$Order == 2) {
    OriginalYTM = B$YieldToMaturity
    OriginalBV = BondValue(B)
    High = OriginalYTM + B$ChangeInYTM
    B$YieldToMaturity = High
    UpBV = BondValue(B)
    Low = OriginalYTM - B$ChangeInYTM
    B$YieldToMaturity = Low
    DownBV = BondValue(B)
    High2 = OriginalYTM + 2*B$ChangeInYTM
    B$YieldToMaturity = High2
    UpBV2 = BondValue(B)
    Low2 = OriginalYTM - 2*B$ChangeInYTM
    B$YieldToMaturity = Low2
    DownBV2 = BondValue(B)
    B$YieldToMaturity = OriginalYTM
    BFD = (-UpBV2 + 8.0*UpBV - 8.0*DownBV + DownBV2) / (12.0*B$ChangeInYTM)
    return( BFD ) 
  } else if (B$Order == 3) {
    OriginalYTM = B$YieldToMaturity
    OriginalBV = BondValue(B)
    High = OriginalYTM + B$ChangeInYTM
    B$YieldToMaturity = High
    UpBV = BondValue(B)
    Low = OriginalYTM - B$ChangeInYTM
    B$YieldToMaturity = Low
    DownBV = BondValue(B)
    High2 = OriginalYTM + 2*B$ChangeInYTM
    B$YieldToMaturity = High2
    UpBV2 = BondValue(B)
    Low2 = OriginalYTM - 2*B$ChangeInYTM
    B$YieldToMaturity = Low2
    DownBV2 = BondValue(B)
    High3 = OriginalYTM + 3*B$ChangeInYTM
    B$YieldToMaturity = High3
    UpBV3 = BondValue(B)
    Low3 = OriginalYTM - 3*B$ChangeInYTM
    B$YieldToMaturity = Low3
    DownBV3 = BondValue(B)
    B$YieldToMaturity = OriginalYTM
    BFD=(UpBV3 - 9.0*UpBV2 + 45.0*UpBV-45.0*DownBV + 9.0*DownBV2 - DownBV3)/
      (60.0*B$ChangeInYTM)
    return( BFD ) 
  } else if (B$Order == 4) {
    OriginalYTM = B$YieldToMaturity
    OriginalBV = BondValue(B)
    High = OriginalYTM + B$ChangeInYTM
    B$YieldToMaturity = High
    UpBV = BondValue(B)
    Low = OriginalYTM - B$ChangeInYTM
    B$YieldToMaturity = Low
    DownBV = BondValue(B)
    High2 = OriginalYTM + 2*B$ChangeInYTM
    B$YieldToMaturity = High2
    UpBV2 = BondValue(B)
    Low2 = OriginalYTM - 2*B$ChangeInYTM
    B$YieldToMaturity = Low2
    DownBV2 = BondValue(B)
    High3 = OriginalYTM + 3*B$ChangeInYTM
    B$YieldToMaturity = High3
    UpBV3 = BondValue(B)
    Low3 = OriginalYTM - 3*B$ChangeInYTM
    B$YieldToMaturity = Low3
    DownBV3 = BondValue(B)
    High4 = OriginalYTM + 4*B$ChangeInYTM
    B$YieldToMaturity = High4
    UpBV4 = BondValue(B)
    Low4 = OriginalYTM - 4*B$ChangeInYTM
    B$YieldToMaturity = Low4
    DownBV4 = BondValue(B)
    B$YieldToMaturity = OriginalYTM
    BFD = ((DownBV4/280.0) - (4.0*DownBV3/105.0) + (DownBV2/5.0) - 
      (4.0*DownBV/5.0) + (4.0*UpBV/5.0) - (UpBV2/5.0) + (4.0*UpBV3/105.0) -
      (UpBV4/280.0)) / B$ChangeInYTM
    return( BFD ) 
  }
}
