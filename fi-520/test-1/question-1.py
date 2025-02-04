import pandas as pd
import numpy as np
from scipy.stats import norm
import math as math
from matplotlib import pyplot as plt
import functions as func

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

# create graph of strike and implied vol
plt.plot(df['Strike_Price'], df['Implied_Vol'], label='Implied Volatility',color='red')
plt.xlabel('Strike Price')
plt.ylabel('Implied Volatility')
plt.title('Strike Price vs Implied Volatility')
plt.legend()
plt.show()





