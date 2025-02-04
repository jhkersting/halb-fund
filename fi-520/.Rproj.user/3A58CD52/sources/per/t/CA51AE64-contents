# empty environment
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.

# load packages
while (!is.null(dev.list()))  dev.off() # Clear old plots
Packages <- c("xts", "MASS", 'lubridate','mvtnorm','hhsmm','parallel')
par(family = 'SF Compact Display') # Globally set fonts for graphs
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# read in functions.R
source("functions-2.R")

# Simulation Information
sims = 1000
n_days = 1 # due to data being one month
start_date = ymd("2024-04-01")
term_length <- 5 # term of swap

# Create Swap Data
swap_data <- list(
  scalar = 2, # Starting Scalar 
  factors = 3, # number of factors
  notional = 1000000, # Notional amount
  start_date = start_date, # Start Date
  current_date = start_date, # Current Date
  end_date = start_date + years(term_length), # End Date
  fix_info = list(
    day_count = '30/360', # day count convention
    pay_freq = 'annual', # payment frequency
    reset_position = 'In Advance', # reset position
    current_rate = 0.05 # will get updated
  ),
  flt_info = list(
    day_count = 'ACT/360', # day count convention
    pay_freq = 'annual', # payment frequency
    reset_position = 'In Advance', # reset position
    current_rate = 0 # null value
  ),
  party_leg = 'floating', # what rate are you paying
  value = 0, # value of swap
  accrued_interest = 0, # accrued interest
  passed_years = 0, # years passed
  years = term_length # years of swap
)


# Get Rates and Rates Data
df <- read.csv("final_data.csv") 
maturities <- c(1 / 12, 3 / 12, 6 / 12, 1, 2, 3, 5, 7, 10, 20, 30)
# remove date column
df
df$date <- as.Date(df$date, format = '%Y-%m-%d')
ts_data <- xts(df[,-1], order.by = df$date)
# plot the 10 year rate

## Clean data
# replace "" with NA
ts_data[ts_data == ''] <- 'ND'
# Remove rows that have ND in every column
ts_data <- ts_data[!apply(ts_data, 1, function(x)all(x == 'ND')),]
# replace 'ND with NA
ts_data[ts_data == 'ND'] <- NA
# remove rows with NA
ts_data <- ts_data[complete.cases(ts_data),]
dates <- index(ts_data)
dates <- ymd(dates)
ts_data
plot(dates,ts_data[,1] , type = 'l', xlab = '10 Year Rate', ylab = 'Date')
# convert to numeric
ts_data <- apply(ts_data, 2, as.numeric)
ts_data <- ts_data / 100

# create changes in rates
ts_data_diff <- diff(ts_data)

# weights divide index by length of data
weights <- (seq_along(ts_data_diff[,1]) / length(ts_data_diff[,1])) ^ (0)
cov_matrix <- cov.wt(ts_data_diff, wt = weights, method = "unbiased")
cov_matrix <- cov_matrix$cov

# get rates on date that matches current date
day_index <- which(dates == swap_data$current_date)
day_index <- as.numeric(day_index)
if (length(day_index) == 0) {
  swap_data$current_date <- dates[which(dates > swap_data$current_date)][1]
  swap_data$start_date <- swap_data$current_date
  swap_data$end_date <- swap_data$current_date + years(term_length)
  day_index <- which(dates == swap_data$current_date)
}
rate_index <- day_index 
### Get Current Rates and Changes
current_rates <- ts_data[rate_index,]
## Get Interest Rate Info
rates_info <- list(rates = current_rates,
                   maturities = maturities,
                   factors = swap_data$factors)
swap_data$yield_curve <- rates_info$rates
rates_info$lsc_coeffs <- get_lsc_coeff(rates_info)

plot(maturities, current_rates*100, type = 'l', xlab = 'Maturity', ylab = 'Rate')
title('Yield Curve for April 1, 2024')


# Calculate Swap info
swap_data$fix_info <- get_leg_info(swap_data, 'fix_info', rates_info)
swap_data$flt_info <- get_leg_info(swap_data, 'flt_info', rates_info)
# get swap fixed rate
fixed_rate <- optimize_fx_rate(swap_data)
swap_data$fixed_rate <- fixed_rate

# plot discount curve
plot(swap_data$flt_info$pay_dates, swap_data$flt_info$discount_factors, type = 'l', xlab = 'Year', ylab = 'Discount Rate',
     sub='10 year Interest rate Swap')
# add title
title('Discount Curve for April 1, 2024')
legend('topright', c('Discount Curve'), col = c('black'), lty = 1)

# plot the flt_info forward rate
plot(swap_data$flt_info$pay_dates,swap_data$flt_info$forward_rates*100,
     type = 'l', xlab = 'Year', ylab = 'Rate',
     sub='10 year Interest rate Swap')
lines(swap_data$flt_info$pay_dates, swap_data$flt_info$lsc_rates*100, col = 'red')
# add swap rate
abline(h = swap_data$fixed_rate*100, col = 'blue')
# add legend
legend('topright', c('Forward Rate', 'LSC Rate', 'Swap Rate'), col = c('black', 'red', 'blue'), lty = 1)
# add title 
title('Rates for Jan. 1, 2024')


# Run VaR 
VaR_output <- run_VaR_simulations(n_days,sims,rates_info,swap_data,cov_matrix,'yes')
s_output <- VaR_output$sim_output
new_rates <- VaR_output$new_rates

# plot current rates
plot(maturities, current_rates*100, type = 'l', xlab = 'Maturity', ylab = 'Rate',
     ylim = c(3, 6))
title('Simulated Yield Curves')
# get first 1000 simulations
new_rates <- new_rates[1:1000,]
# sort by average rate
new_rates <- new_rates[order(apply(new_rates, 1, mean)),]
# add first 10 simulations with rainbow colors
for (i in 1:1000) {
  lines(maturities,new_rates[i,]*100, col = rainbow(1000)[i], lty = 2)
}
lines(maturities, current_rates*100, col = 'black', lty = 1, lwd = 3)


# create density lines for the 30 year rate
densities <- apply(new_rates, 2, density)
densities
max_rate <- max(new_rates)
min_rate <- min(new_rates)
max_density <- max(sapply(densities, function(x)
  max(x$y)))

plot(
  densities[[1]],
  type = 'l',
  xlab = 'Rate',
  ylab = 'Density',
  col = rainbow(11)[1],
  lty = 1,
  lwd = 2,
  xlim = c(min_rate, max_rate),
  ylim = c(0, max_density),
  main = 'Density of Simulated Rates for Jan. 31, 2024'
)

for (i in 2:11) {
  lines(densities[[i]],
        col = rainbow(11)[i],
        lty = 1,
        lwd = 2)
}
# add legend
legend(
  'topleft',
  c(
    '1 Month',
    '3 Month',
    '6 Month',
    '1 Year',
    '2 Year',
    '3 Year',
    '5 Year',
    '7 Year',
    '10 Year',
    '20 Year',
    '30 Year'
  ),
  col = rainbow(11),
  lty = 1
)
title('Density of Simulated Rates for Jan. 31, 2024')

print(paste("The Fixed Rate is: ", swap_data$fixed_rate))
print(paste("The 95% VaR is: ",VaR_output$VaR[2,2]))


