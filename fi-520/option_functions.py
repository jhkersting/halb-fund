import numpy as np
import scipy.stats as si

def euro_binomial_model(S, K, T, r, sigma, option_type, N):
    delta_t = T / N
    u = np.exp(sigma * np.sqrt(delta_t))
    d = 1 / u
    p = (np.exp(r * delta_t) - d) / (u - d)
    q = 1 - p
    stock_price = np.zeros((N + 1, N + 1))
    option_price = np.zeros((N + 1, N + 1))
    for i in range(N + 1):
        for j in range(i + 1):
            stock_price[j, i] = S * (u ** (i - j)) * (d ** j)
    if option_type == "call":
        option_price[:, N] = np.maximum(np.zeros(N + 1), stock_price[:, N] - K)
    else:
        option_price[:, N] = np.maximum(np.zeros(N + 1), K - stock_price[:, N])
    print(option_price)
    for i in range(N - 1, -1, -1): # Start, stop, step
        for j in range(i + 1):

            option_price[j, i] = np.exp(-r * delta_t) * (p * option_price[j, i + 1] + q * option_price[j + 1, i + 1])
    return option_price[0, 0]


def american_binomial_model(S, K, T, r, sigma, option_type, N):
    delta_t = T / N
    u = np.exp(sigma * np.sqrt(delta_t))
    d = 1 / u
    p = (np.exp(r * delta_t) - d) / (u - d)
    q = 1 - p
    stock_price = np.zeros((N + 1, N + 1))
    option_price = np.zeros((N + 1, N + 1))
    early_exercise = np.zeros((N + 1, N + 1))
    for i in range(N + 1):
        for j in range(i + 1):
            stock_price[j, i] = S * (u ** (i - j)) * (d ** j)
    if option_type == "call":
        option_price[:, N] = np.maximum(np.zeros(N + 1), stock_price[:, N] - K)
    else:
        option_price[:, N] = np.maximum(np.zeros(N + 1), K - stock_price[:, N])
    for i in range(N - 1, -1, -1): # Start, stop, step
        for j in range(i + 1):
            if option_type == "call":
                option_price[j, i] = np.maximum(np.exp(-r * delta_t) * (p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]),
                                                stock_price[j, i] - K)
                early_exercise[j, i] = 1 if stock_price[j, i] - K > np.exp(-r * delta_t) * (
                            p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]) else 0
            else:
                option_price[j, i] = np.maximum(np.exp(-r * delta_t) * (p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]),
                                                K - stock_price[j, i])
                early_exercise[j, i] = 1 if K - stock_price[j, i] > np.exp(-r * delta_t) * (
                            p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]) else 0
    return option_price[0, 0]


print(euro_binomial_model(100,100,.0849,0.05,0.2,"put",3))