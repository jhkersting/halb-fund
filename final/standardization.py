import pandas as pd
import yfinance as yf
import statsmodels.api as sm
import numpy as np
import os
import time

df = pd.read_csv('returns.csv')
ff5 = pd.read_csv('~/Desktop/fi-596/ff5-mom-adj.csv')
components = pd.read_csv('compenents.csv')
#components = pd.read_csv('~/Desktop/fi-596/components.csv')
#components['ticker'] = components['Symbol']
# replace . with - in the ticker
components['ticker'] = components['ticker'].str.replace('.','-')
df['date'] = pd.to_datetime(df['date'])
ff5['date'] = pd.to_datetime(ff5['date'], format='%Y%m%d')
ff5['year'] = ff5['date'].dt.year
factors = ['mkt-rf', 'smb', 'hml', 'rmw', 'cma', 'mom', 'alpha']  # factors to test
regression_factors = ['mkt-rf', 'smb', 'hml', 'rmw', 'cma', 'mom'] # fama french factors

#df = df.merge(ff5, on='date', how='left')

years = np.linspace(2000, 2023, 24)  # years to run the backtest
years = years.astype(int)  # convert the years to integers

components = list(components['ticker'].unique())

regression_output = pd.DataFrame()
for y in years:
    dat = df[df['year'] == y].copy()
    ff5_year = ff5[ff5['year'] == y].copy()
    for t in components:
        print(t)
        try:
            dat_t = dat[['date',t]]
            # rename the columns
            dat_t.columns = ['date','return']
            dat_t = dat_t.dropna()
            if len(dat_t) == len(ff5_year):
                dat_t = dat_t.merge(ff5_year, on='date', how='left')
                # Run regression
                adj_return = dat_t['return'] - dat_t['rf']
                x = sm.add_constant(dat_t[regression_factors])

                model = sm.OLS(adj_return, x).fit()
                for f in regression_factors:
                    coeff = model.params[f]  # get the coefficient
                    t_val = model.tvalues[f]  #
                    o = pd.DataFrame([{'ticker': t, 'factor': f, 'year': y, 'coefficient': coeff, 't-value': t_val}])
                    regression_output = pd.concat([regression_output, o])
                # Do another output for the intercept
                coeff = model.params['const']  # get the coefficient
                t_val = model.tvalues['const']  # get the t-value
                o = pd.DataFrame([{'ticker': t, 'factor': 'alpha', 'year': y, 'coefficient': coeff, 't-value': t_val}])
                regression_output = pd.concat([regression_output, o])
                # do another output for the total return
                total_return = (1 + dat_t['return']).prod() - 1
                o = pd.DataFrame(
                    [{'ticker': t, 'factor': 'total_return', 'year': y, 'coefficient': total_return, 't-value': 1}])
                regression_output = pd.concat([regression_output, o])
                print(t,y)
                #regression_output.to_csv('regression-output.csv',index=False)
        except KeyError:
            print('Error:',t,y)

regression_output.round(5).to_csv('regression-output.csv',index=False)


factors = ['mkt-rf', 'smb', 'hml', 'rmw', 'cma', 'mom', 'alpha']  # factors to test

df = regression_output
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