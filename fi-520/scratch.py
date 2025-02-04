import pandas as pd
import numpy as np
from scipy.stats import norm
import math as math
from matplotlib import pyplot as plt


df = pd.read_csv('put-option.csv')

print(df.head())

stock = 402
rf = 0.041
T = 0.85
div = 0.0074
sigma = 0.2
strike = 400


def black_scholes_model(s, x, t, r, div, sigma, option_type):
    d1 = (np.log(s / x) + (r - div + (sigma ** 2) / 2) * t) / (sigma * np.sqrt(t))
    d2 = d1 - sigma * np.sqrt(t)
    nd1 = norm.cdf(d1)
    nd2 = norm.cdf(d2)
    if option_type == "call":
        return s * np.exp(-div * t) * nd1 - x * np.exp(-r * t) * nd2
    else:
        return x * np.exp(-r * t) * (1 - nd2) - s * np.exp(-div * t) * (1 - nd1)




def implied_vol_function(s0, x, t, rc, price, tol, div_y, iterations=1000):
    vol_0 = 0.0001
    current_change = .01
    lower_bound = max(0, s0 - np.exp(-rc * t) * x)
    for i in range(iterations):
        p = black_scholes_model(s0, x, t, rc, div_y, vol_0, "put")
        if abs(price - p) < tol:
            return vol_0
        if p < price:
            vol_0 += current_change
        if p > price:
            vol_0 -= current_change
            current_change /= 10


for i, d in df.iterrows():
    implied_vol = implied_vol_function(stock, d['Strike_Price'], T, rf, d['Price'], 0.0001, div)
    df.at[i, 'Implied_Vol'] = implied_vol

df.round(5).to_csv('put-option-implied-vol.csv', index=False)

# create graph of strike and implied vol
plt.plot(df['Strike_Price'], df['Implied_Vol'], label='Implied Volatility',color='red')
plt.xlabel('Strike Price')
plt.ylabel('Implied Volatility')
plt.title('Strike Price vs Implied Volatility')
plt.legend()
plt.show()



# create model to calcuate future spot price given random walk
def stock_random_walk(s0, rf, div, sigma, t, n):
    delta_t = t / n
    stock_price = np.zeros(n + 1)
    stock_price[0] = s0
    for i in range(1, n + 1):
        stock_price[i] = stock_price[i - 1] * np.exp(
            (rf - div - (sigma ** 2) / 2) * delta_t + sigma * np.sqrt(delta_t) * np.random.normal(0, 1))
    return stock_price[-1]


