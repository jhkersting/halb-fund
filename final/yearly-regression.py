import pandas as pd
import yfinance as yf
import statsmodels.api as sm
import numpy as np
import os
import time

factors = ['mkt-rf', 'smb', 'hml', 'rmw', 'cma', 'mom', 'alpha']  # factors to test

df = pd.read_csv('regression-output.csv')

years = np.linspace(2000, 2023, 24)  # years to run the backtest
years = years.astype(int)  # convert the years to integers


all_returns = pd.read_csv('returns.csv')
ff5 = pd.read_csv('~/Desktop/fi-596/ff5-mom-adj.csv')
ff5['date'] = pd.to_datetime(ff5['date'], format='%Y%m%d')  # convert the date to datetime
all_returns['date'] = pd.to_datetime(all_returns['date'])
all_returns = all_returns.merge(ff5, on='date', how='left')
print(all_returns)
for y in years:
    # create directory if it does not exist
    if not os.path.exists(f'{y}'):
        os.makedirs(f'{y}')
    dat = df[df['year'] == y].copy()  # read the data
    dat.round(5).to_csv(f'{y}/regression-output.csv', index=False)
    # get the returns for the year
    returns = all_returns[all_returns['year'] == y]
    # remove columns that have NaNs
    returns = returns.dropna(axis=1, how='any')
    returns.round(5).to_csv(f'{y}/returns-ff5-mom.csv', index=False)
    for f in factors:
        if not os.path.exists(f'{y}/{f}'):
            os.makedirs(f'{y}/{f}')