compound_option <- function(S, K, T, r, sigma, option_type, N, div_y, x2, t2, option_type2) {
  delta_t <- T / N
  N2 <- as.integer(t2 / T * N)
  u <- exp(sigma * sqrt(delta_t))
  d <- 1 / u
  p <- (exp((r - div_y) * delta_t) - d) / (u - d)
  q <- 1 - p
  
  stock_price <- matrix(0, nrow = N + 1, ncol = N + 1)
  option_price <- matrix(0, nrow = N + 1, ncol = N + 1)
  compound_price <- matrix(0, nrow = N2 + 1, ncol = N2 + 1)
  
  for (i in 0:N) {
    for (j in 0:i) {
      stock_price[j + 1, i + 1] <- S * (u^(i - j)) * (d^j)
    }
  }
  
  if (option_type == "call") {
    option_price[, N + 1] <- pmax(0, stock_price[, N + 1] - K)
  } else {
    option_price[, N + 1] <- pmax(0, K - stock_price[, N + 1])
  }
  
  for (i in N:1) {
    for (j in 0:(i - 1)) {
      option_price[j + 1, i] <- exp(-r * delta_t) * (p * option_price[j + 1, i + 1] + q * option_price[j + 2, i + 1])
    }
  }
  
  # Restrict the matrix size to N2 + 1 for compound calculations
  option_price <- option_price[1:(N2 + 1), 1:(N2 + 1)]
  
  if (option_type2 == "call") {
    compound_price[, N2 + 1] <- pmax(0, option_price[, N2 + 1] - x2)
  } else {
    compound_price[, N2 + 1] <- pmax(0, x2 - option_price[, N2 + 1])
  }
  
  for (i in N2:1) {
    for (j in 0:(i - 1)) {
      compound_price[j + 1, i] <- exp(-r * delta_t) * (p * compound_price[j + 1, i + 1] + q * compound_price[j + 2, i + 1])
    }
  }
  
  return(compound_price[1, 1])
}
