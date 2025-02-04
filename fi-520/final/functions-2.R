get_pay_freq_numeric <- function(PF) {
  if (PF == 'quarterly') {
    return(0.25)
  }
  if (PF == 'semiannual') {
    return(0.5)
  }
  if (PF == 'annual') {
    return(1)
  }
}

get_pay_dates <- function(SDAY, EDAY, PF) {
  # create a sequence of dates from start_date to end_date
  if (PF == 'quarterly') {
    inc <- "3 months"
  }
  if (PF == 'semiannual') {
    inc <- "6 months"
  }
  if (PF == 'annual') {
    inc <- "1 year"
  }
  dates1 <- seq(SDAY, EDAY, by = inc)
  dates <- dates1[-1]
  return(dates)
}

get_reset_dates <-
  function(SDAY, EDAY, info) {
    # SDAY = start date, EDAY = end date,
    # info = flt_info
    # create a sequence of dates from start_date to end_date
    PF <- info$pay_freq
    RP <- info$reset_position
    if (PF == 'quarterly') {
      inc <- "3 months"
    }
    if (PF == 'semiannual') {
      inc <- "6 months"
    }
    if (PF == 'annual') {
      inc <- "1 year"
    }
    dates1 <- seq(SDAY, EDAY, by = inc)
    if (RP == 'In Advance') {
      dates <- dates1[-length(dates1)]
    } else {
      dates <- dates1[-1]
    }
    return(dates)
  }


get_lsc_scalar <- function(Sc, Rs, Ms, Fa) {
  x2 <- (Sc / Ms) * (1 - exp(-Ms / Sc))
  x3 <- x2 - exp(-Ms / Sc)
  x4 <- x3
  model <- lm(Rs ~ x2 + x3)
  ll <- -logLik(model)  # Negate log-likelihood for minimization
  return(as.numeric(ll))  # Ensure the return value is numeric for optimization
}


optimize_scalar <- function(Rs, Ms, Fa, initial_scalar) {
  # The optim function to find the optimal scalar
  optimal_result <- optim(
    par = initial_scalar,
    fn = get_lsc_scalar,
    Rs = Rs,
    Ms = Ms,
    Fa = Fa,
    method = "L-BFGS-B",
    # Suitable for bounded optimization
    lower = 0.01,
    # Lower bound for the scalar
    upper = 100
  )          # Upper bound for the scalar
  par <- optimal_result$par  # Get the optimal scalar found
  return(par)  # Return the optimal scalar found
}

get_lsc_coeff <- function(input) {
  Rs <- input$rates
  Ms <- input$maturities
  Fa <- input$factors
  Sc <- optimize_scalar(Rs, Ms, Fa, 1)
  x2 <- (Sc / Ms) * (1 - exp(-Ms / Sc))
  x3 <- x2 - exp(-Ms / Sc)
  model <- lm(Rs ~ x2 + x3)
  coeff <- coef(model)
  scalar <- Sc
  # create a list with the scalar and coefficients
  output <- list(scalar = scalar, coefficients = coeff)
  return(output)
}


get_lsc_rate <- function(input, M, Fa) {
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

get_lsc_rates <-function(Rinfo, input) {
    # Rinfo = Rate Info, input = leg_info
    Rs <- Rinfo$rates # Rs is rates
    Ms <- Rinfo$maturities # Ms is maturities
    lsc_input <- Rinfo$lsc_coeffs
    periods <- input$pay_years
    # create a double with the discount rates
    lsc_rates <-sapply(periods, function(x)
        get_lsc_rate(lsc_input, x, Fa))
    return(as.numeric(lsc_rates))
  }

get_forward_rates <-function(Rinfo, input) {
    # Rinfo = Rate Info, input = leg_info
    Rs <- Rinfo$rates # Rs is rates
    Ms <- Rinfo$maturities # Ms is maturities
    RP <- input$reset_position # RP is reset position
    lsc_input <- Rinfo$lsc_coeffs
    if (RP == 'In Advance') {
      periods <- input$pay_years
      start_period <- 0
      periods <- c(start_period, periods)
      lsc_rates <- sapply(periods, function(x)
        get_lsc_rate(lsc_input, x, Fa)) 
    } 
    else {
      periods <- input$pay_years
      final_period <- max(periods) + input$pay_freq_numeric
      periods <- c(periods, final_period)
      lsc_rates <- sapply(periods, function(x)
        get_lsc_rate(lsc_input, x, Fa))
    }
    lsc_rates <- as.numeric(lsc_rates * input$pay_freq_numeric)
    forwards <- c()
    for (i in 1:length(lsc_rates)) {
      # get the forward rate
      a <- (1 + lsc_rates[i]) ^ i
      b <- (1 + lsc_rates[i - 1]) ^ (i - 1)
      f <- (a / b) - 1
      forwards <- c(forwards, f)
    }
    forwards <- forwards / input$pay_freq_numeric
    
    return(as.numeric(forwards))
  }

get_discount_factors <- function(Rinfo, input) {
    # Rinfo = Rate Info, input = leg_info
    Rs <- Rinfo$rates # Rs is rates
    Ms <- Rinfo$maturities # Ms is maturities
    lsc_input <- Rinfo$lsc_coeffs
    periods <- input$pay_years
    # create a double with the discount rates
    lsc_rates <-sapply(periods, function(x)
      get_lsc_rate(lsc_input, x, Fa))
    #lsc_rates <- lsc_rates * input$pay_freq_numeric
    discount_factors <- c()
    for (i in 1:length(lsc_rates)) {
      time_period <- periods[i]
      df <- 1 / (1 + lsc_rates[i] ) ^ time_period
      discount_factors <- c(discount_factors, df)
    }
    write.csv(discount_factors, file = "discounts.csv")
    return(as.numeric(discount_factors))
}

get_leg_info <- function(input, leg,Rinfo) {
  info <- input[[leg]]
  # change name of list to info
  SD <- input$start_date # SD = start date
  CD <- input$current_date # CD = current date
  ED <- input$end_date # ED = end date
  info$pay_dates <- get_pay_dates(SD, ED, info$pay_freq)
  info$pay_freq_numeric <- get_pay_freq_numeric(info$pay_freq)
  info$reset_dates <- get_reset_dates(SD, ED, info)
  info$pay_years <- interval(CD, info$pay_dates) / years(1)
  info$reset_years <- interval(CD, info$reset_dates) / years(1)
  if (info$day_count == '30/360') {
    info$period_length <-
      rep(info$pay_freq_numeric, length(info$reset_dates))
    info$period_length[1] <-  info$period_length[1] - input$passed_years * (360/365.25)
  }
  else{
    info$period_length <- diff(info$pay_years)
    info$period_length <- c(info$pay_years[1], info$period_length)
  }
  info$lsc_rates <- get_lsc_rates(Rinfo, info)
  info$forward_rates <- get_forward_rates(Rinfo, info)
  info$discount_factors <- get_discount_factors(Rinfo, info)
  return(info)
}

get_swap_rate <- function(input){
  dfs <- input$flt_info$discount_factors
  flt_cfs <- input$flt_info$forward_rates * input$flt_info$period_length 
  if(input$flt_info$day_count == '30/360'){
    flt_cfs <- flt_cfs * (360 / 365.25)
  }
  else{
    flt_cfs <- flt_cfs * (365.25/360)
  }
  weights <- dfs / sum(dfs)
  swap_rate <- sum(flt_cfs * weights)
  swap_rate <- swap_rate / input$flt_info$pay_freq_numeric
  return(as.numeric(swap_rate))
}



get_accrued_interest <- function(new,og){
  # input is a list with fix_info and flt_info
  fix_rate <- og$fixed_rate
  fix_accrued <- fix_rate * og$notional * new$passed_years
  flt_rate <- og$flt_info$forward_rates[1]
  flt_accrued <- flt_rate * og$notional * new$passed_years
  accrued_interest <- flt_accrued - fix_accrued
  return(as.numeric(accrued_interest))
}

get_swap_value <- function(input){
  # get pv of fixed leg
  fixed_rate <- input$fixed_rate
  fixed_day_count <- input$fix_info$day_count
  if(fixed_day_count == '30/360'){
    fixed_rate <- fixed_rate * (360 / 365.25)
  }
  fixed_rates <- input$fix_info$period_length * fixed_rate 
  fixed_cfs <- fixed_rates * input$notional
  fixed_pv <- sum(fixed_cfs * input$fix_info$discount_factors)

  # get pv of floating leg
  flt_rates <- input$flt_info$forward_rates * input$flt_info$period_length 
  flt_day_count <- input$flt_info$day_count
  if(flt_day_count == '30/360'){
    flt_rates <- flt_rates * (360 / 365.25)
  }
  flt_cfs <- flt_rates * input$notional
  flt_pv <- sum(flt_cfs * input$flt_info$discount_factors)

  # get swap value (fixed payer cash flow)
  swap_value <- flt_pv - fixed_pv + input$accrued_interest
  if(input$party_leg == 'floating'){
    swap_value <- swap_value * -1
  }
  return(swap_value)
}


get_swap_rate_opt <- function(fixed_rate,input) {
  # Correct the variable naming mistake for 'fix_period'
  fix_period <- input$fix_info$period_length
  if( input$fix_info$day_count == '30/360'){
    fix_period <- fix_period * (360 / 365.25)
  }
  fix_cf <- fix_period * fixed_rate
  fix_pv <- fix_cf * input$fix_info$discount_factors
  fix_pv <- sum(fix_pv)
  flt_period <- input$flt_info$period_length
  if( input$flt_info$day_count == '30/360'){
    flt_period <- flt_period * (360 / 365.25)
  }
  
  flt_rates <- input$flt_info$forward_rates * flt_period
  flt_cfs <- flt_rates
  flt_pv <- flt_cfs * input$flt_info$discount_factors
  flt_pv <- sum(flt_pv)
  diff <- flt_pv - fix_pv
  ll <- abs(diff)
  return(ll)
}


optimize_fx_rate <- function(input,fixed_rate=0.1) {
  # The optim function to find the optimal scalar
  optimal_result <- optim(
    par = fixed_rate,
    fn = get_swap_rate_opt,
    input = input,
    method = "L-BFGS-B",
    # Suitable for bounded optimization
    lower = 0.001,
    # Lower bound for the scalar
    upper = .20
  )          # Upper bound for the scalar
  par <- optimal_result$par  # Get the optimal scalar found
  return(par)  # Return the optimal scalar found
}


run_VaR_simulations <- function(N,SIMS,Rinfo,SWAP,CovMat,plot_density='no'){ # N = no_days, SIMS = no_sims,
  # CRs = current_rates, SWAP = swap_data, MA = maturities
  # Simulate VaR
  CRs <- Rinfo$rates
  Ma <- Rinfo$maturities
  sim_output <- numeric(sims)
  new_rates_matrix <- matrix(0, nrow = SIMS, ncol = length(CRs))
  n_days <- N
  # create place to store sum of changes

  for (i in 1:SIMS) {
    changes <- mvrnorm(n = N, mu = rep(0, length(CRs)), Sigma = CovMat)
    if(n_days >1){
      changes <- apply(changes, 2, sum)
    }
    # sum changes for each rate
    new_rates <- CRs + changes
    # get new date
    new_date <- SWAP$current_date + days(n_days)
    # get new rate info
    sim_info <- list(rates = new_rates,
                     maturities = Ma,
                     current_date = new_date)
    # get current lsc coeff
    sim_info$lsc_coeffs <- get_lsc_coeff(sim_info)
    # swap data in an updated list
    updated_swap <- SWAP
    updated_swap$yield_curve <- sim_info$rates
    updated_swap$current_date <- sim_info$current_date
    updated_swap$years <-
      interval(updated_swap$start_date, updated_swap$end_date) / years(1)
    updated_swap$passed_years <-
      interval(swap_data$start_date,updated_swap$current_date) / years(1)
    
    # get leg info
    updated_swap$fix_info <- get_leg_info(updated_swap, 'fix_info', sim_info)
    updated_swap$flt_info <- get_leg_info(updated_swap, 'flt_info', sim_info)
    
    # get_accrued_interest
    updated_swap$accrued_interest <- get_accrued_interest(updated_swap,swap_data)
    updated_swap_value <- get_swap_value(updated_swap)
    new_rates_matrix[i,] <- new_rates
    sim_output[i] <- updated_swap_value
  }
  # get 90%,95%,99% VaR
  VaR_90 <- quantile(sim_output, 0.1)
  VaR_95 <- quantile(sim_output, 0.05)
  VaR_99 <- quantile(sim_output, 0.01)
  VaR_double <- c(VaR_90, VaR_95, VaR_99)
  Var_names <- c("VaR_90", "VaR_95", "VaR_99")
  VaR <- data.frame(Var_names, VaR_double)
  # average if below VaR
  avg_90 <- mean(sim_output[sim_output < VaR_90])
  avg_95 <- mean(sim_output[sim_output < VaR_95])
  avg_99 <- mean(sim_output[sim_output < VaR_99])
  # create a list with VaR and average
  avg_double <- c(avg_90, avg_95, avg_99)
  avg_names <- c("Average_90", "Average_95", "Average_99")
  avg <- data.frame(avg_names, avg_double)
  average_value <- mean(sim_output)
  sd_value <- sd(sim_output)
  output <- list(VaR = VaR, Average = avg, Average_Value = average_value,
                 SD_Value = sd_value, sim_output = sim_output, new_rates = new_rates_matrix)
  if(plot_density == 'yes'){
    # plot density with 50 bins
    # create a max value for the x-axis
    max_value <-  200000
    min_value <- -200000
    options(scipen = 999)
    sim_output <- sim_output[sim_output < max_value]
    sim_output <- sim_output[sim_output > min_value]
    hist(sim_output, main = "Density of Swap Value", xlab = "Swap Value",
         breaks = 50, col = "lightblue", border = "black")
  }
  return(output)
  
}








