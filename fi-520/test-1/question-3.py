import numpy as np
import functions as func

s0 = 119
rf = 0.042
vol = .45
div = 0.015
t = 1
future = s0 * np.exp((rf - div) * t)
print(f'Future Price: {future}')
x = 140
option_type = 'call'

# Euro call on future with same expiration is the same as a call on the stock
euro_call = func.euro_binomial_model_future(s0, x, t, rf, vol, option_type, 100, div)
print(f'Euro Call: {euro_call}')
american_call = func.american_binomial_model_future(s0, x, t, rf, vol, option_type, 4, div)
print(f'American Call: {american_call}')