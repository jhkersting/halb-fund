# Replication of Figure 1.R
# CF0Q0 - Missing RAND Code
# CF0Q1 - Quintile 1 RAND Code (no observations)
# CF0Q2 - Quintile 2 RAND Code
# CF0Q3 - Quintile 3 RAND Code
# CF0Q4 - Quintile 4 RAND Code
# CF0Q5 - Quintile 5 RAND Code
#
# Quintile 2
CF0Q2M2 = CF0Q2[-c(16, 17),]
LengthRow <- length(CF0Q2M2[1,]) - 1 # Less Industry column
LengthCol <- length(CF0Q2M2$Industry)
CF0Q2M2$TotalCF <- 0
for(i in 1:LengthCol){
  for(j in 2:(LengthRow+1)){ # Dataframes: [Column, Row]
    CF0Q2M2$TotalCF[i] <- CF0Q2M2$TotalCF[i] + as.numeric(CF0Q2M2[i,j])
  }
}
CF0Q2M2$TotalValue <- CF0Q2M2$TotalCF * II$PE
CF0Q2TV <- sum(CF0Q2M2$TotalValue)
CF0Q2M2[nrow(CF0Q2M2) + 1,] = 1 #nrow: Number of rows
CF0Q2M2[nrow(CF0Q2M2),1] <- "Manufacturing"
for(i in 2:ncol(CF0Q2M2)){
  CF0Q2M2[nrow(CF0Q2M2),i] <- as.numeric(CF0Q2M2[2,i]) + 
    + as.numeric(CF0Q2M2[3,i]) + as.numeric(CF0Q2M2[4,i]) +
    + as.numeric(CF0Q2M2[5,i]) + as.numeric(CF0Q2M2[6,i]) +
    + as.numeric(CF0Q2M2[7,i]) + as.numeric(CF0Q2M2[8,i]) +
    + as.numeric(CF0Q2M2[9,i]) 
}
keepRow <- c("1", "10", "11", "12", "13", "14", "15", "16")
# keepCol <- c("Industry", "TotalCF", "TotalValue")
Q2Summary <- CF0Q2M2[keepRow,] 
LastRow <- nrow(Q2Summary)+1
LastCol <- ncol(Q2Summary)
Q2Summary[LastRow,1] <- "Total"
Q2Summary[LastRow,LastCol-1]  <- 0.0 # Delete NAs
Q2Summary[LastRow,LastCol]  <- 0.0
Q2Summary[LastRow,LastCol-1]  <- sum(Q2Summary$TotalCF)
Q2Summary[LastRow,LastCol]  <- sum(Q2Summary$TotalValue)
#
# Quintile 3
#
CF0Q3M2 = CF0Q3[-c(16, 17),]
LengthRow <- length(CF0Q3M2[1,]) - 1 # Less Industry column
LengthCol <- length(CF0Q3M2$Industry)
CF0Q3M2$TotalCF <- 0
for(i in 1:LengthCol){
  for(j in 2:(LengthRow+1)){ # Dataframes: [Column, Row]
    CF0Q3M2$TotalCF[i] <- CF0Q3M2$TotalCF[i] + as.numeric(CF0Q3M2[i,j])
  }
}
CF0Q3M2$TotalValue <- CF0Q3M2$TotalCF * II$PE
CF0Q3TV <- sum(CF0Q3M2$TotalValue)
CF0Q3M2[nrow(CF0Q3M2) + 1,] = 1 #nrow: Number of rows
CF0Q3M2[nrow(CF0Q3M2),1] <- "Manufacturing"
for(i in 2:ncol(CF0Q3M2)){
  CF0Q3M2[nrow(CF0Q3M2),i] <- as.numeric(CF0Q3M2[2,i]) + 
    + as.numeric(CF0Q3M2[3,i]) + as.numeric(CF0Q3M2[4,i]) +
    + as.numeric(CF0Q3M2[5,i]) + as.numeric(CF0Q3M2[6,i]) +
    + as.numeric(CF0Q3M2[7,i]) + as.numeric(CF0Q3M2[8,i]) +
    + as.numeric(CF0Q3M2[9,i]) 
}
keepRow <- c("1", "10", "11", "12", "13", "14", "15", "16")
# keepCol <- c("Industry", "TotalCF", "TotalValue")
Q3Summary <- CF0Q3M2[keepRow,] 
LastRow <- nrow(Q3Summary)+1
LastCol <- ncol(Q3Summary)
Q3Summary[LastRow,1] <- "Total"
Q3Summary[LastRow,LastCol-1]  <- 0.0 # Delete NAs
Q3Summary[LastRow,LastCol]  <- 0.0
Q3Summary[LastRow,LastCol-1]  <- sum(Q3Summary$TotalCF)
Q3Summary[LastRow,LastCol]  <- sum(Q3Summary$TotalValue)
#
# Quintile 4
#
CF0Q4M2 = CF0Q4[-c(16, 17),]
LengthRow <- length(CF0Q4M2[1,]) - 1 # Less Industry column
LengthCol <- length(CF0Q4M2$Industry)
CF0Q4M2$TotalCF <- 0
for(i in 1:LengthCol){
  for(j in 2:(LengthRow+1)){ # Dataframes: [Column, Row]
    CF0Q4M2$TotalCF[i] <- CF0Q4M2$TotalCF[i] + as.numeric(CF0Q4M2[i,j])
  }
}
CF0Q4M2$TotalValue <- CF0Q4M2$TotalCF * II$PE
CF0Q4TV <- sum(CF0Q4M2$TotalValue)
CF0Q4M2[nrow(CF0Q4M2) + 1,] = 1 #nrow: Number of rows
CF0Q4M2[nrow(CF0Q4M2),1] <- "Manufacturing"
for(i in 2:ncol(CF0Q4M2)){
  CF0Q4M2[nrow(CF0Q4M2),i] <- as.numeric(CF0Q4M2[2,i]) + 
    + as.numeric(CF0Q4M2[3,i]) + as.numeric(CF0Q4M2[4,i]) +
    + as.numeric(CF0Q4M2[5,i]) + as.numeric(CF0Q4M2[6,i]) +
    + as.numeric(CF0Q4M2[7,i]) + as.numeric(CF0Q4M2[8,i]) +
    + as.numeric(CF0Q4M2[9,i]) 
}
keepRow <- c("1", "10", "11", "12", "13", "14", "15", "16")
# keepCol <- c("Industry", "TotalCF", "TotalValue")
Q4Summary <- CF0Q4M2[keepRow,] 
LastRow <- nrow(Q4Summary)+1
LastCol <- ncol(Q4Summary)
Q4Summary[LastRow,1] <- "Total"
Q4Summary[LastRow,LastCol-1]  <- 0.0 # Delete NAs
Q4Summary[LastRow,LastCol]  <- 0.0
Q4Summary[LastRow,LastCol-1]  <- sum(Q4Summary$TotalCF)
Q4Summary[LastRow,LastCol]  <- sum(Q4Summary$TotalValue)
# Combine three groups and rename columns
names(Q2Summary)[names(Q2Summary)=="TotalCF"] <- "TotalCF2"
names(Q2Summary)[names(Q2Summary)=="TotalValue"] <- "TotalValue2"
names(Q3Summary)[names(Q3Summary)=="TotalCF"] <- "TotalCF3"
names(Q3Summary)[names(Q3Summary)=="TotalValue"] <- "TotalValue3"
names(Q4Summary)[names(Q4Summary)=="TotalCF"] <- "TotalCF4"
names(Q4Summary)[names(Q4Summary)=="TotalValue"] <- "TotalValue4"
# Consolidate Figure 1
Figure1Summary <- merge(Q2Summary, Q3Summary, by = 'Industry', sort = FALSE)
Figure1Summary <- merge(Figure1Summary, Q4Summary, by = 'Industry', 
  sort = FALSE)
write.xlsx(Figure1Summary, "Global Outbreaks Figure 1 File.xlsx", 
  sheetName = "Figure 1 Summary")



# vlookup_df(c('Mining'), CF0Q4M2, lookup_column = 'Industry', 
#  result_column = 'TotalValue')
# dict = data.frame(num=1:26, small=letters, cap=LETTERS, 
#   stringsAsFactors = FALSE)
# vlookup_df(1:3, dict)
# dict[1:3,]
# identical(vlookup_df(1:3, dict), dict[1:3,]) # should be TRUE
# rownames(dict) = paste0('rows', 1:26)
# vlookup(c(45,1:3,58), dict, result_column='cap')
# vlookup_df(c('z','d','f'), dict, lookup_column = 'small')
# vlookup_df(c('rows7', 'rows2', 'rows5'), dict, lookup_column = 'row.names')

