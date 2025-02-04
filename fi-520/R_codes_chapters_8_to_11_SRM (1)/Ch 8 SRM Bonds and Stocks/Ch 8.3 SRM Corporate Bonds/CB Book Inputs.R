# CB Book Inputs.R
#  Contains both individual CB and CMT curve information
# Read CB bonds from spreadsheet
CB <- read.xlsx(xlsxFile = CBFileName, sheet = 1, skipEmptyRows = FALSE)
# is.data.frame(CB)
CB$BID <- NULL # Remove bid price
CB$CHG <- NULL # Remove price change from previous day
CB$JMaturityDate = as.date(CB$MATURITY - 21916) # Create Julian date
CB$MATURITY <- NULL # Remove spreadsheet maturity
CB$MaturityDate <- date.mmddyyyy(CB$JMaturityDate) # Create R date
CB$JMaturityDate <- as.integer(CB$JMaturityDate) # Make visible on R page
# Work on creating quoted bond price in decimal form
APrice0 <- CB$ASKED
APrice <- trunc(CB$ASKED) # Truncate price
AFrac1 <- round((APrice0 - APrice)*100) # Number of 32nds
AFrac2 <- APrice0*1000 - APrice*1000 - AFrac1*10 # Number of 8ths
CB$APrice <- APrice + (AFrac1 + AFrac2/8)/32 # Price in decimal form, % of par


