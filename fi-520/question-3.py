import numpy as np

# Input parameters
initial_stock_price = 210
risk_free_rate = 0.042
volatility = 0.25
strike_price = 190
time_to_maturity = 1.2
option_type = "put"
dividend_yield = 0.02
compound_option_type = 'put'
compound_strike_price = 15
compound_time_to_maturity = 0.75
number_steps = 100

def compound_option(stock_price, strike, maturity, interest_rate, sigma, opt_type, steps, div_yield, strike2, maturity2, opt_type2):
    """
    Calculate the price of a compound option using a binomial tree model.

    Parameters:
        stock_price (float): Initial stock price.
        strike (float): Strike price of the first option.
        maturity (float): Time to expiration of the first option.
        interest_rate (float): Risk-free interest rate.
        sigma (float): Volatility of the stock.
        opt_type (str): Type of the first option ('call' or 'put').
        steps (int): Number of steps in the binomial tree.
        div_yield (float): Dividend yield of the stock.
        strike2 (float): Strike price of the second option.
        maturity2 (float): Time to expiration of the second option.
        opt_type2 (str): Type of the second option ('call' or 'put').

    Returns:
        float: Price of the compound option.
    """
    delta_t = maturity / steps
    steps2 = int(maturity2 / maturity * steps)
    up_factor = np.exp(sigma * np.sqrt(delta_t))
    down_factor = 1 / up_factor
    up_prob = (np.exp((interest_rate - div_yield) * delta_t) - down_factor) / (up_factor - down_factor)
    down_prob = 1 - up_prob

    # Initialize matrices for stock prices, option prices at each node
    stock_prices = np.zeros((steps + 1, steps + 1))
    option_prices = np.zeros((steps + 1, steps + 1))
    compound_option_prices = np.zeros((steps2 + 1, steps2 + 1))

    # Generate stock prices in binomial tree
    for i in range(steps + 1):
        for j in range(i + 1):
            stock_prices[j, i] = stock_price * (up_factor ** (i - j)) * (down_factor ** j)

    # Calculate option prices at maturity
    if opt_type == "call":
        option_prices[:, steps] = np.maximum(0, stock_prices[:, steps] - strike)
    else:
        option_prices[:, steps] = np.maximum(0, strike - stock_prices[:, steps])

    # Backward calculation for option prices
    for i in range(steps - 1, -1, -1):
        for j in range(i + 1):
            option_prices[j, i] = np.exp(-interest_rate * delta_t) * (up_prob * option_prices[j, i + 1] + down_prob * option_prices[j + 1, i + 1])

    # Restrict option prices for the compound option calculation
    restricted_option_prices = option_prices[:steps2 + 1, :steps2 + 1]

    # Calculate compound option prices at the second maturity
    if opt_type2 == "call":
        compound_option_prices[:, steps2] = np.maximum(0, restricted_option_prices[:, steps2] - strike2)
    else:
        compound_option_prices[:, steps2] = np.maximum(0, strike2 - restricted_option_prices[:, steps2])

    # Backward calculation for compound option prices
    for i in range(steps2 - 1, -1, -1):
        for j in range(i + 1):
            compound_option_prices[j, i] = np.exp(-interest_rate * delta_t) * (up_prob * compound_option_prices[j, i + 1] + down_prob * compound_option_prices[j + 1, i + 1])

    return compound_option_prices[0, 0]

# Calculate initial compound option value
initial_value = compound_option(
    initial_stock_price, strike_price, time_to_maturity, risk_free_rate,
    volatility, option_type, number_steps, dividend_yield, compound_strike_price,
    compound_time_to_maturity, compound_option_type)

# Calculate Delta and Vega for sensitivity analysis
h = 0.01
delta = (compound_option(initial_stock_price + h, strike_price, time_to_maturity, risk_free_rate, volatility, option_type, 300, dividend_yield, compound_strike_price, compound_time_to_maturity, compound_option_type) -
         compound_option(initial_stock_price - h, strike_price, time_to_maturity, risk_free_rate, volatility, option_type, 300, dividend_yield, compound_strike_price, compound_time_to_maturity, compound_option_type)) / (2 * h)

vol_h = 0.001
vega = (compound_option(initial_stock_price, strike_price, time_to_maturity, risk_free_rate, volatility + vol_h, option_type, 300, dividend_yield, compound_strike_price, compound_time_to_maturity, compound_option_type) -
        compound_option(initial_stock_price, strike_price, time_to_maturity, risk_free_rate, volatility - vol_h, option_type, 300, dividend_yield, compound_strike_price, compound_time_to_maturity, compound_option_type)) / (2 * vol_h)

# Calculate the new value of the compound option based on changed market conditions
new_stock_price = 220
new_volatility = 0.27
value_change = delta * (new_stock_price - initial_stock_price) + vega * (new_volatility - volatility)
new_value = initial_value + value_change

print(new_value)
print('Delta:', delta)
print('Vega:', vega)