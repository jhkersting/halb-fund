# LSC Model Bar Charts.R

barplot(II$Price, names.arg = II$Ticker, xlab = "Industries", ylab = "Price ($)")
barplot(II$PCF, names.arg = II$Ticker, xlab = "Industries", ylab = "Price/CF")
barplot(II$GLevel, names.arg = II$Ticker, xlab = "Industries", ylab = "GLevel (%)")
barplot(II$GSlope, names.arg = II$Ticker, xlab = "Industries", ylab = "GSlope (%)")
barplot(II$DRLevel, names.arg = II$Ticker, xlab = "Industries", ylab = "DRLevel (%)")
barplot(II$DRSlope, names.arg = II$Ticker, xlab = "Industries", ylab = "DRSlope (%)")
barplot(II$DeltaLg, names.arg = II$Ticker, xlab = "Industries", ylab = "DeltaLg")
barplot(II$DeltaPCFLg, names.arg = II$Ticker, xlab = "Industries", ylab = "DeltaPCFLg")
barplot(II$DeltaSg, names.arg = II$Ticker, xlab = "Industries", ylab = "DeltaSg")
barplot(II$DeltaPCFSg, names.arg = II$Ticker, xlab = "Industries", ylab = "DeltaPCFSg")
barplot(II$DeltaLf, names.arg = II$Ticker, xlab = "Industries", ylab = "DeltaLf")
barplot(II$DeltaPCFLf, names.arg = II$Ticker, xlab = "Industries", ylab = "DeltaPCFLf")
barplot(II$DeltaSf, names.arg = II$Ticker, xlab = "Industries", ylab = "DeltaSf")
barplot(II$DeltaPCFSf, names.arg = II$Ticker, xlab = "Industries", ylab = "DeltaPCFSf")

