get_lsc_scalar <- function(Sc,Rinfo) {
  Rs <- Rinfo$rates
  Ms <- Rinfo$maturities
  x2 <- (Sc / Ms) * (1 - exp(-Ms / Sc))
  x3 <- x2 - exp(-Ms / Sc)
  x4 <- x3
  model <- lm(Rs ~ x2 + x3)
  ll <- -logLik(model)  # Negate log-likelihood for minimization
  return(as.numeric(ll))  # Ensure the return value is numeric for optimization
}

optimize_scalar <- function(Rinfo, initial_scalar = 1) {
  # The optim function to find the optimal scalar
  optimal_result <- optim(
    par = initial_scalar,
    fn = get_lsc_scalar,
    Rinfo = Rinfo,
    method = "L-BFGS-B",
    # Suitable for bounded optimization
    lower = 0.01,
    # Lower bound for the scalar
    upper = 100
  )          # Upper bound for the scalar
  par <- optimal_result$par  # Get the optimal scalar found
  return(par)  # Return the optimal scalar found
}

get_lsc_coeffs <- function(input) {
  Rs <- input$rates
  Ms <- input$maturities
  Sc <- optimize_scalar(input, 1)
  x2 <- (Sc / Ms) * (1 - exp(-Ms / Sc))
  x3 <- x2 - exp(-Ms / Sc)
  model <- lm(Rs ~ x2 + x3)
  coeff <- coef(model)
  scalar <- Sc
  # create a list with the scalar and coefficients
  output <- list(scalar = scalar, coefficients = coeff)
  return(output)
}


get_lsc_rate <- function(input, M) {
  # input is a list with scalar and coefficients, M is maturity, Fa is factors
  coeffs <- input$coefficients
  Sc <- input$scalar
  M <- ifelse(M == 0, 0.00001, M)
  x2 <- (Sc / M) * (1 - exp(-M / Sc))
  x3 <- x2 - exp(-M / Sc)
  c1 <- coeffs[1]
  c2 <- coeffs[2]
  c3 <- coeffs[3]
  rate <- c1 + c2 * x2 + c3 * x3
  return(rate)
}

get_current_lsc_rates <- function(input) {
  lsc_input <- input$lsc_coeffs
  periods <- seq(0, 30, by = 1/12)
  print(periods)
  # create a double with the discount rates
  lsc_rates <-sapply(periods, function(x)
    get_lsc_rate(lsc_input, x))
  return(as.numeric(lsc_rates))
}

get_bond_value <- function(input){
  discount_factors <- as.numeric(1 / (1 + input$lsc_rates) ^ input$coupon_numerical)
  discounted_cash_flows <- input$coupon_payments * discount_factors
  discounted_principal <- input$face_value * discount_factors[length(discount_factors)]
  bond_value <- sum(discounted_cash_flows) + discounted_principal
  return(bond_value)
}

get_deriv_level <- function(rINFO, bINFO, h){
  # Derivative of the bond price with respect to the level
  NRI <- rINFO # rate info
  NBI <- bINFO # bond info
  # get change in bond (uptick)
  NRI$lsc_coeffs$coefficients[1] <- NRI$lsc_coeffs$coefficients[1] + h
  NBI$lsc_rates <- sapply(NBI$coupon_numerical, function(x)
    get_lsc_rate(NRI$lsc_coeffs, x))
  price_up <- get_bond_value(NBI)
  # get change in bond (downtick)
  NRI$lsc_coeffs$coefficients[1] <- NRI$lsc_coeffs$coefficients[1] - h * 2
  NBI$lsc_rates <- sapply(NBI$coupon_numerical, function(x)
    get_lsc_rate(NRI$lsc_coeffs, x))
  price_down <- get_bond_value(NBI)
  # return the derivative
  return((price_up - price_down) / (2 * h))
}

get_deriv_slope <- function(rINFO, bINFO, h){
  # Derivative of the bond price with respect to the slope
  NRI <- rINFO # rate info
  NBI <- bINFO # bond info
  # get change in bond (uptick)
  NRI$lsc_coeffs$coefficients[2] <- NRI$lsc_coeffs$coefficients[2] + h
  NBI$lsc_rates <- sapply(NBI$coupon_numerical, function(x)
    get_lsc_rate(NRI$lsc_coeffs, x))
  price_up <- get_bond_value(NBI)
  # get change in bond (downtick)
  NRI$lsc_coeffs$coefficients[2] <- NRI$lsc_coeffs$coefficients[2] - h * 2
  NBI$lsc_rates <- sapply(NBI$coupon_numerical, function(x)
    get_lsc_rate(NRI$lsc_coeffs, x))
  price_down <- get_bond_value(NBI)
  # return the derivative
  return((price_up - price_down) / (2 * h))
}

get_deriv_curve <- function(rINFO, bINFO, h){
  # Derivative of the bond price with respect to the curvature
  NRI <- rINFO # rate info
  NBI <- bINFO # bond info
  # get change in bond (uptick)
  NRI$lsc_coeffs$coefficients[3] <- NRI$lsc_coeffs$coefficients[3] + h
  NBI$lsc_rates <- sapply(NBI$coupon_numerical, function(x)
    get_lsc_rate(NRI$lsc_coeffs, x))
  price_up <- get_bond_value(NBI)
  # get change in bond (downtick)
  NRI$lsc_coeffs$coefficients[3] <- NRI$lsc_coeffs$coefficients[3] - h * 2
  NBI$lsc_rates <- sapply(NBI$coupon_numerical, function(x)
    get_lsc_rate(NRI$lsc_coeffs, x))
  price_down <- get_bond_value(NBI)
  # return the derivative
  return((price_up - price_down) / (2 * h))
}
