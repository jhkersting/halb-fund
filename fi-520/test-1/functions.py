import numpy as np
from scipy.stats import norm


def bsmovm(s, x, t, r, div, sigma, option_type):
    d1 = (np.log(s / x) + (r - div + (sigma ** 2) / 2) * t) / (sigma * np.sqrt(t))
    d2 = d1 - sigma * np.sqrt(t)
    nd1 = norm.cdf(d1)
    nd2 = norm.cdf(d2)
    if option_type == "call":
        return s * np.exp(-div * t) * nd1 - x * np.exp(-r * t) * nd2
    else:
        return x * np.exp(-r * t) * (1 - nd2) - s * np.exp(-div * t) * (1 - nd1)

def binary_option(s, x, t, r, div, sigma):
    d1 = (np.log(s / x) + (r - div + (sigma ** 2) / 2) * t) / (sigma * np.sqrt(t))
    d2 = d1 - sigma * np.sqrt(t)
    return np.exp(-r * t) * norm.cdf(d2)


def find_implied_vol(s0, x, t, rc, price, tol, div_y, iterations=1000):
    vol_0 = 0.0001
    current_change = .01
    lower_bound = max(0, s0 - np.exp(-rc * t) * x)
    for i in range(iterations):
        p = bsmovm(s0, x, t, rc, div_y, vol_0, "put")
        if abs(price - p) < tol:
            return vol_0
        if p < price:
            vol_0 += current_change
        if p > price:
            vol_0 -= current_change
            current_change /= 10


def euro_binomial_model(S, K, T, r, sigma, option_type, N, div_y):
    delta_t = T / N
    u = np.exp(sigma * np.sqrt(delta_t))
    d = 1 / u
    p = (np.exp((r - div_y) * delta_t) - d) / (u - d)
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
    for i in range(N - 1, -1, -1):  # Start, stop, step
        for j in range(i + 1):
            option_price[j, i] = np.exp(-r * delta_t) * (p * option_price[j, i + 1] + q * option_price[j + 1, i + 1])
    return option_price[0, 0]


def compound_option(S, K, T, r, sigma, option_type, N, div_y, x2, t2, option_type2):
    delta_t = T / N
    N2 = int(t2 / T * N)
    u = np.exp(sigma * np.sqrt(delta_t))
    d = 1 / u
    p = (np.exp((r - div_y) * delta_t) - d) / (u - d)
    q = 1 - p
    stock_price = np.zeros((N + 1, N + 1))
    option_price = np.zeros((N + 1, N + 1))
    compound_price = np.zeros((N2 + 1, N2 + 1))
    for i in range(N + 1):
        for j in range(i + 1):
            stock_price[j, i] = S * (u ** (i - j)) * (d ** j)
    if option_type == "call":
        option_price[:, N] = np.maximum(np.zeros(N + 1), stock_price[:, N] - K)
    else:
        option_price[:, N] = np.maximum(np.zeros(N + 1), K - stock_price[:, N])
    for i in range(N - 1, -1, -1):  # Start, stop, step
        for j in range(i + 1):
            option_price[j, i] = np.exp(-r * delta_t) * (p * option_price[j, i + 1] + q * option_price[j + 1, i + 1])
    # get first n2 + 1 rows and n2 + 1 columns of
    option_price = option_price[:N2 + 1, :N2 + 1]
    if option_type2 == "call":
        compound_price[:, N2] = np.maximum(np.zeros(N2 + 1), option_price[:, N2] - x2)
    else:
        compound_price[:, N2] = np.maximum(np.zeros(N2 + 1), x2 - option_price[:, N2])
    for i in range(N2 - 1, -1, -1):  # Start, stop, step
        for j in range(i + 1):
            compound_price[j, i] = np.exp(-r * delta_t) * (
                        p * compound_price[j, i + 1] + q * compound_price[j + 1, i + 1])
    return compound_price[0, 0]


def american_binomial_model(S, K, T, r, sigma, option_type, N, div_y):
    delta_t = T / N
    u = np.exp(sigma * np.sqrt(delta_t))
    d = 1 / u
    p = (np.exp((r - div_y) * delta_t) - d) / (u - d)
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
    for i in range(N - 1, -1, -1):  # Start, stop, step
        for j in range(i + 1):
            if option_type == "call":
                option_price[j, i] = np.maximum(
                    np.exp(-r * delta_t) * (p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]),
                    stock_price[j, i] - K)
                early_exercise[j, i] = 1 if stock_price[j, i] - K > np.exp(-r * delta_t) * (
                        p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]) else 0
            else:
                option_price[j, i] = np.maximum(
                    np.exp(-r * delta_t) * (p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]),
                    K - stock_price[j, i])
                early_exercise[j, i] = 1 if K - stock_price[j, i] > np.exp(-r * delta_t) * (
                        p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]) else 0
    return option_price[0, 0]


def euro_binomial_model_future(S, K, T, r, sigma, option_type, N, div_y):
    delta_t = T / N
    u = np.exp(sigma * np.sqrt(delta_t))
    d = 1 / u
    p = (np.exp((r - div_y) * delta_t) - d) / (u - d)
    q = 1 - p
    stock_price = np.zeros((N + 1, N + 1))
    option_price = np.zeros((N + 1, N + 1))
    future_price = np.zeros((N + 1, N + 1))
    # calculate stock price
    for i in range(N + 1):
        for j in range(i + 1):
            stock_price[j, i] = S * (u ** (i - j)) * (d ** j)
    # calculate future price
    for i in range(N + 1):
        for j in range(i + 1):
            future_price[j, i] = stock_price[j, i] * np.exp((r - div_y) * (N - i) * delta_t)
    # get option price at expiration
    if option_type == "call":
        option_price[:, N] = np.maximum(np.zeros(N + 1), future_price[:, N] - K)
    else:
        option_price[:, N] = np.maximum(np.zeros(N + 1), K - future_price[:, N])
    # calculate option price at each node
    for i in range(N - 1, -1, -1):  # Start, stop, step
        for j in range(i + 1):
            option_price[j, i] = np.exp(-r * delta_t) * (p * option_price[j, i + 1] + q * option_price[j + 1, i + 1])
    return option_price[0, 0]


def american_binomial_model_future(S, K, T, r, sigma, option_type, N, div_y):
    delta_t = T / N
    u = np.exp(sigma * np.sqrt(delta_t))
    d = 1 / u
    p = (np.exp((r - div_y) * delta_t) - d) / (u - d)
    q = 1 - p
    stock_price = np.zeros((N + 1, N + 1))
    option_price = np.zeros((N + 1, N + 1))
    early_exercise = np.zeros((N + 1, N + 1))
    future_price = np.zeros((N + 1, N + 1))
    # calculate stock price
    for i in range(N + 1):
        for j in range(i + 1):

            stock_price[j, i] = S * (u ** (i - j)) * (d ** j) * np.exp((r - div_y) * (N - j + 1) * delta_t)
    # calculate future price
    for i in range(N + 1):
        for j in range(i + 1):
            future_price[j, i] = stock_price[j, i] * np.exp((r - div_y) * (N - i) * delta_t)
    if option_type == "call":
        option_price[:, N] = np.maximum(np.zeros(N + 1), future_price[:, N] - K)
    else:
        option_price[:, N] = np.maximum(np.zeros(N + 1), K - future_price[:, N])
    for i in range(N - 1, -1, -1):  # Start, stop, step
        for j in range(i + 1):
            if option_type == "call":
                option_price[j, i] = np.maximum(
                    np.exp(-r * delta_t) * (p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]),
                    future_price[j, i] - K)
                early_exercise[j, i] = 1 if future_price[j, i] - K > np.exp(-r * delta_t) * (
                        p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]) else 0
            else:
                option_price[j, i] = np.maximum(
                    np.exp(-r * delta_t) * (p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]),
                    K - future_price[j, i])
                early_exercise[j, i] = 1 if K - future_price[j, i] > np.exp(-r * delta_t) * (
                        p * option_price[j, i + 1] + q * option_price[j + 1, i + 1]) else 0
    return option_price[0, 0]



