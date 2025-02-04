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
# read in functions code
source('functions-1.R')
# rate infos
maturities <- c(1/12,3/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30)
df <- read.csv('final_data.csv')
rates <- as.numeric(df[1,2:12]/100)
spread <- 0.0116
bbb_rates <- rates + spread
rate_info <- list(rates = bbb_rates, maturities = maturities,
                  spread = spread)


rate_info$lsc_coeffs <- get_lsc_coeffs(rate_info)
rate_info$lsc_coeffs$scalar <- 2

# plot the current LSC curve
lsc_curve_rates <- get_current_lsc_rates(rate_info)
lsc_cuve_mat <- seq(0, 30, by = 1/12)
plot(lsc_cuve_mat, lsc_curve_rates*100, type = 'l', xlab = 'Maturity (years)', ylab = 'Rate (%)', main = 'Current LSC Curve')
points(maturities, rate_info$rates*100, col = 'red', pch = 19)

## Bond Info
bond_info <- list(
  face_value = 1000,
  maturity_date = ymd('2037-04-15'),
  coupon_rate = 0.0675,
  coupon_freq = 2,
  issue_date = ymd('2022-04-15'),# used for calculating coupon dates
  today = ymd('2024-04-30'), 
  term_length = 15
)
bond_info$coupon_payments <- bond_info$face_value * bond_info$coupon_rate / bond_info$coupon_freq

bond_info$coupon_dates <- seq(bond_info$issue_date, bond_info$maturity_date, by = "6 months")
# remove first coupon date if it has passed
bond_info$coupon_dates <- bond_info$coupon_dates[bond_info$coupon_dates > bond_info$today]
bond_info$coupon_numerical <- as.numeric(bond_info$coupon_dates - bond_info$today) / 365.25
bond_info$lsc_rates <- sapply(bond_info$coupon_numerical, function(x)
  get_lsc_rate(rate_info$lsc_coeffs, x))
bond_info$price <- get_bond_value(bond_info)
PRICE <- bond_info$price

# get derivatives
h <- 0.0001
# Derivative of the bond price with respect to the level
d_level <- get_deriv_level(rate_info, bond_info, h)
# Derivative of the bond price with respect to the slope
d_slope <- get_deriv_slope(rate_info, bond_info, h)
# Derivative of the bond price with respect to the curvature
d_curve <- get_deriv_curve(rate_info, bond_info, h)


# Changes in lsc factors
dLEVEL = -.007
dSLOPE = 0
dCURVE = 0.040


# change derived from derivatives
dPRICE = dLEVEL * d_level + dSLOPE * d_slope + dCURVE * d_curve
new_price = PRICE + dPRICE
print(new_price)



# actual changes
NRI <- rate_info
NBI <- bond_info
NRI$lsc_coeffs$coefficients[1] <- NRI$lsc_coeffs$coefficients[1] + dLEVEL
NRI$lsc_coeffs$coefficients[2] <- NRI$lsc_coeffs$coefficients[2] + dSLOPE
NRI$lsc_coeffs$coefficients[3] <- NRI$lsc_coeffs$coefficients[3] + dCURVE
NBI$lsc_rates <- sapply(NBI$coupon_numerical, function(x)
  get_lsc_rate(NRI$lsc_coeffs, x))
# plot NBI curve
max <- max(NBI$lsc_rates*100, bond_info$lsc_rates*100)
min <- min(NBI$lsc_rates*100, bond_info$lsc_rates*100)
plot(NBI$coupon_numerical, NBI$lsc_rates*100, type = 'l', xlab = 'Maturity (years)', ylab = 'Rate (%)', main = 'Current LSC Curve',
     ylim = c(min, max))
# add old curve
lines(bond_info$coupon_numerical, bond_info$lsc_rates*100, col = 'red', pch = 19)
legend('topright', c('New Curve', 'Old Curve'), col = c('black', 'red'), lty = 1, pch = 19)
new_price_actual <- get_bond_value(NBI)

print(paste('Original Price:', PRICE))
print(paste('Derivative of level:', d_level))
print(paste('Derivative of slope:', d_slope))
print(paste('Derivative of curvature:', d_curve))
print(paste('Price Change:', dPRICE))
print(paste('New Price (Using Derivatives):', new_price))
print(paste('New Price (Actual):', new_price_actual))









