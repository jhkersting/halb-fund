
STOCK = 100
STRIKE = 100
RISK_FREE_RATE = 0.05
VOLATILITY = 0.2
YEARS = 0.5
STEPS = 3

# Calc Date
calc_day <- 1
calc_month <- 1
calc_year <- 2020

# Expiration Date
exp_day <- 1
exp_month <- 2
exp_year <- 2020

# Convert dates to Date objects
calc_date <- as.Date(paste(calc_year, calc_month, calc_day, sep = "-"))
exp_date <- as.Date(paste(exp_year, exp_month, exp_day, sep = "-"))
# Calculate time to expiration
TIME_TO_EXPIRATION <- as.numeric(difftime(exp_date, calc_date, units = "days")) / 365


# Binomial model for European options
euroBinomialModel <- function(S, K, T, r, sigma, optionType, N) {
  deltaT <- T / N
  u <- exp(sigma * sqrt(deltaT))
  d <- 1 / u
  p <- (exp(r * deltaT) - d) / (u - d)
  q <- 1 - p
  
  stockPrice <- matrix(0, nrow = N + 1, ncol = N + 1)
  optionPrice <- matrix(0, nrow = N + 1, ncol = N + 1)
  
  for (i in 0:N) {
    for (j in 0:i) {
      stockPrice[j + 1, i + 1] <- S * (u ^ (i - j)) * (d ^ j)
    }
  }
  
  if (optionType == "call") {
    optionPrice[, N + 1] <- pmax(0, stockPrice[, N + 1] - K)
  } else {
    optionPrice[, N + 1] <- pmax(0, K - stockPrice[, N + 1])
  }
  
  for (i in N:1) {
    for (j in 0:(i - 1)) {
      optionPrice[j + 1, i] <- exp(-r * deltaT) * (p * optionPrice[j + 1, i + 1] + q * optionPrice[j + 2, i + 1])
    }
  }
  
  return(optionPrice[1, 1])
}


# Binomial model for European options (both options)
euroBinomialModelBoth <- function(S, K, T, r, sigma, N) {
  deltaT <- T / N
  u <- exp(sigma * sqrt(deltaT))
  d <- 1 / u
  p <- (exp(r * deltaT) - d) / (u - d)
  q <- 1 - p
  
  stockPrice <- matrix(0, nrow = N + 1, ncol = N + 1)
  callPrice <- matrix(0, nrow = N + 1, ncol = N + 1)
  putPrice <- matrix(0, nrow = N + 1, ncol = N + 1)
  
  for (i in 0:N) {
    for (j in 0:i) {
      stockPrice[j + 1, i + 1] <- S * (u ^ (i - j)) * (d ^ j)
    }
  }
  

  callPrice[, N + 1] <- pmax(0, stockPrice[, N + 1] - K)
  putPrice[, N + 1] <- pmax(0, K - stockPrice[, N + 1])
  
  
  for (i in N:1) {
    for (j in 0:(i - 1)) {
      callPrice[j + 1, i] <- exp(-r * deltaT) * (p * callPrice[j + 1, i + 1] + q * callPrice[j + 2, i + 1])
      putPrice[j + 1, i] <- exp(-r * deltaT) * (p * putPrice[j + 1, i + 1] + q * putPrice[j + 2, i + 1])
    }
  }
  types <- c("call", "put")
  values <- c(callPrice[1, 1], putPrice[1, 1])
  output <- data.frame(types, values)
  
  return(output)
}

optionValues <- euroBinomialModelBoth(STOCK, STRIKE, TIME_TO_EXPIRATION, RISK_FREE_RATE, VOLATILITY, STEPS)






