import pandas as pd
import yfinance as yf
import statsmodels.api as sm
import numpy as np
import os
import time

start = time.time()
df = pd.read_csv('compenents.csv')
#df = df.head()
# replace . with - in the ticker
df['ticker'] = df['ticker'].str.replace('.','-')
print(df)

years = np.linspace(2000, 2023, 24)  # years to run the backtest
years = years.astype(int)  # convert the years to integers

spy = yf.download('^GSPC', start="2000-01-01", end="2024-01-01",progress=False)
df0 =  spy['Adj Close'].pct_change().dropna()
df0 = df0.to_frame()
df0['^GSPC'] = df0['Adj Close']
df0['year'] = df0.index.year
df0['date'] = df0.index
df0 = df0[['date','^GSPC','year']]
days_per_year = list(df0.groupby('year').count()['date'])


for i,d in df.iterrows():
    try:
        ticker = d['ticker']
        data = yf.download(ticker, start="2000-01-01", end="2024-01-01",progress=False)
        data = data['Adj Close'].pct_change().dropna()
        data = data.to_frame()
        data['year'] = data.index.year
        data['date'] = data.index
        data[ticker] = data['Adj Close']
        years_included = []
        for y in years:
            num_days = len(data[data['year'] == y])
            index = years.tolist().index(y)
            if num_days == days_per_year[index]:
                years_included.append(y)
        info = data[['date',ticker]]
        df0 = pd.merge(df0, info, on='date', how='left')
        print(ticker,i)
    except:
        print('Error:',ticker)
        continue


df0.round(4).to_csv('returns.csv',index=False)
print('Time to run:', time.time()-start)
