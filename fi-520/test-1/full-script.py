import pandas as pd
import numpy as np
from scipy.stats import norm
import math as math
from matplotlib import pyplot as plt
import functions as func

## Question 1
print('Question 1')
df = pd.read_csv('put-option.csv')

stock = 402
rf = 0.041
T = 0.85
div = 0.0074
sigma = 0.2
strike = 400


for i, d in df.iterrows():
    implied_vol = func.find_implied_vol(stock, d['Strike_Price'], T, rf, d['Price'], 0.0001, div)
    implied_vol = round(implied_vol, 5)
    print(f'Implied Volatility: {implied_vol} for Strike Price: {d["Strike_Price"]}')
    df.at[i, 'Implied_Vol'] = implied_vol

df.round(5).to_csv('put-option-output.csv', index=False)
# Graph at end of script

## Question 2
print('')
print('Question 2')
s = 200
rf = 0.042
vol = 0.30
x = 180
t = 1
option_type = "put"
div_y = 0.02

compound_type = 'call'
compound_x = 5
compound_t = 0.5


val = func.compound_option(s, x, t, rf, vol, option_type, 1000, div_y, compound_x, compound_t, compound_type)
print(f'Compound Option: {val}')

## Question 3
print('')
print('Question 3')

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

## Question 4
print('')
print('Question 4')

simulations = 100000
s = 40
vol = .4
rf = .045
t = 10
div = 0

# first option buying 0.4 calls at strike of 4
first_option = func.bsmovm(s, 4, t, rf, div, vol, 'call') * 0.4
print(f'First Option: {first_option}')
# second option writing 0.4 calls at strike of 10
second_option = -func.bsmovm(s, 10, t, rf, div, vol, 'call') * 0.4
print(f'Second Option: {second_option}')
# third option binary call at 30
third_option = -func.binary_option(s, 30, t, rf, div, vol) * 2.4
print(f'Third Option: {third_option}')
# value of security
value = first_option + second_option + third_option
print(f'Price of Security: {value}')




# Graph from question 1
plt.plot(df['Strike_Price'], df['Implied_Vol'], label='Implied Volatility',color='red')
plt.xlabel('Strike Price')
plt.ylabel('Implied Volatility')
plt.title('Strike Price vs Implied Volatility')
plt.legend()
plt.show()