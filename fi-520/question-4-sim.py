import numpy as np

simulations = 100000
s = 40
vol = .4
r = .045
t = 10
div = 0

def future_stock_price(s0, r, div, sigma, t, n):
    delta_t = t / n
    stock_price = np.zeros(n + 1)
    stock_price[0] = s0
    for i in range(1, n + 1):
        stock_price[i] = stock_price[i - 1] * np.exp(
            (r - div - (sigma ** 2) / 2) * delta_t + sigma * np.sqrt(delta_t) * np.random.normal(0, 1))
    return stock_price[-1]


price_output = []
value_output = []
for i in range(simulations):
    price = future_stock_price(s, r, div, vol, t, 1)
    if price < 4:
        value = 0
    elif 10 > price > 4:
        value = (price - 4) * .4
    elif 30 > price > 10:
        value = 2.4
    else:
        value = 0
    
    price_output.append(price)
    value_output.append(value)

avg_price = np.mean(price_output)
value = np.mean(value_output) * np.exp(-r * t)
print('Average Price: '+avg_price)
print('Price of Security: '+value)



