import pandas as pd
import statsmodels.api as sm
import numpy as np
import yfinance as yf

sectors_data = pd.read_csv('sector_industry.csv')

sectors = list(sectors_data['sector'].unique())

years = np.linspace(2001,2023,23)
years = years.astype(int)

output = pd.DataFrame()
for y in years:
    portfolio = pd.read_csv(str(y) + '/mkt-rf-alpha/portfolio.csv')
    portfolio['sector'] = portfolio['ticker'].map(sectors_data.set_index('ticker')['sector'])
    for s in sectors:
        weight = portfolio[portfolio['sector'] == s]['weight_z-score'].sum()
        beta_weight = portfolio[portfolio['sector'] == s]['beta'] * portfolio[portfolio['sector'] == s]['weight_z-score']
        beta = beta_weight.sum() / weight
        o = pd.DataFrame({'year': [y], 'sector': [s], 'weight': [weight], 'beta': [beta]})
        output =pd.concat([output,o])
    print(portfolio.head())

output.round(4).to_csv('yearly-sector-weights.csv', index=False)




