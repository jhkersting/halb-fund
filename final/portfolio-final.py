import pandas as pd
import yfinance as yf
import statsmodels.api as sm
import numpy as np
import os
import time

years = np.linspace(2000, 2023, 24)  # years to run the backtest
years = years.astype(int)  # convert the years to integers
factors = ['mkt-rf-alpha']  # factors to test
regression_factors = ['mkt-rf', 'smb', 'hml', 'rmw', 'cma', 'mom']  # fama french factors
store_columns = ['mkt-rf', 'smb', 'hml', 'rmw', 'cma', 'mom', 'date', 'rf']  # columns to store the returns
direction = ''  # direction of weight (empty for forward)
print(store_columns)
factors0 = ['mkt-rf', 'alpha']

# Standardize the factors
standardized_data = pd.DataFrame()  # create a dataframe to store the output for each year
for y in years:
    dat = pd.read_csv(f'{y}/regression-output.csv')  # read the data
    dat = dat[dat['ticker'] != 'ORLA']  # remove ORLA
    # remove column if its name is 'IVT'
    output = pd.DataFrame()
    for f in factors0:
        f_dat = dat[dat['factor'] == f].copy()  # get the data for the factor
        # f_dat = f_dat.head(500)
        std = f_dat['coefficient'].std()
        avg = f_dat['coefficient'].median()
        if f == 'mkt-rf':
            f_dat['z-score'] = -((f_dat['coefficient'] - avg) / std)
        else:
            f_dat['z-score'] = (f_dat['coefficient'] - avg) / std
        # standardized_data = pd.concat([standardized_data, f_dat])
        # create directory if it does not exist

        if (f == 'alpha') or (f == 'mkt-rf'):
            output = pd.concat([output, f_dat])
    output['z-score'] = output['z-score'].apply(lambda x: 0 if x < 0 else x)
    comps = list(output['ticker'].unique())
    for c in comps:
        dat_c = output[output['ticker'] == c].copy()
        sum_z = dat_c['z-score'].prod()
        beta = dat_c[dat_c['factor'] == 'mkt-rf']['coefficient'].values[0]
        beta_z = dat_c[dat_c['factor'] == 'mkt-rf']['z-score'].values[0]
        alpha = dat_c[dat_c['factor'] == 'alpha']['coefficient'].values[0]
        alpha_z = dat_c[dat_c['factor'] == 'alpha']['z-score'].values[0]
        o = pd.DataFrame([{'ticker': c, 'factor': 'mkt-rf-alpha', 'coefficient': sum_z, 'z-score': sum_z,
                           't-value': 1, 'year': y, 'beta': beta, 'alpha': alpha}])
        standardized_data = pd.concat([standardized_data, o])

components = standardized_data['ticker'].unique()
newdf = pd.DataFrame()
for c in components:
    print(c)
    dat = standardized_data[standardized_data['ticker'] == c].copy()
    for f in factors:
        dat_f = dat[dat['factor'] == f].copy()
        z = -dat_f['z-score'].shift(1) if direction == '-backwards' else dat_f['z-score'].shift(1)
        t = -dat_f['t-value'].shift(1) if direction == '-backwards' else dat_f['t-value'].shift(1)
        alpha = dat_f['alpha'].shift(1)
        beta = dat_f['beta'].shift(1)
        # if values are greater than 5, set them to 5
        z = z.apply(lambda x: 5 if x > 5 else x)
        dat_f['last_z-score'] = z
        dat_f['last_t-value'] = 1
        dat_f['last_alpha'] = alpha
        dat_f['last_beta'] = beta
        newdf = pd.concat([newdf, dat_f])

# remove year of 2000
standardized_data = newdf[newdf['year'] != 2000]
print(standardized_data.head())

# Get weights
calc_years = years[1:]  # years to run the backtest
df = pd.DataFrame()
for y in calc_years:
    dat = standardized_data[standardized_data['year'] == y].copy()
    for f in factors:
        print(f, y, 'weighting')
        dat_f = dat[dat['factor'] == f].copy()
        dat_f['weight_z-score'] = dat_f['last_z-score'].apply(lambda x: 0 if x < 0 else x)
        dat_f['weight_t-value'] = dat_f['last_t-value'].apply(lambda x: 0 if x < 0 else x)
        dat_f['weight_equal'] = 1 / len(dat_f)
        sum_weight_z = dat_f['weight_z-score'].sum()
        sum_weight_t = dat_f['weight_t-value'].sum()
        dat_f['weight_z-score'] = dat_f['weight_z-score'] / sum_weight_z
        dat_f['weight_t-value'] = dat_f['weight_t-value'] / sum_weight_t
        # Reweigh firms with too high weights
        dat_f['weight_z-score'] = dat_f['weight_z-score'].apply(lambda x: 0 if x < 0 else x)
        dat_f['weight_z-score'] = dat_f['weight_z-score'].apply(lambda x: 0.1 if x > 0.1 else x)
        sum_z = dat_f['weight_z-score'].sum()
        # Second reweighing
        dat_f['weight_z-score'] = dat_f['weight_z-score'] / sum_z
        dat_f['weight_z-score'] = dat_f['weight_z-score'].apply(lambda x: 0.1 if x > 0.1 else x)
        sum_z = dat_f['weight_z-score'].sum()
        dat_f['weight_z-score'] = dat_f['weight_z-score'] / sum_z
        df = pd.concat([df, dat_f])

# remove na
df = df.dropna()
# Send the data to a csv file
df.round(5).to_csv(f'standardized-regression-output.csv', index=False)

# Run the backtest
year_data = pd.DataFrame()  # create a dataframe to store the output for each year
portfolio_output = pd.DataFrame()  # create a dataframe to store the output for each year
weights = ['z-score', 't-value', 'equal']
for y in calc_years:
    dat = df[df['year'] == y].copy()  # get the data for the year
    all_returns = pd.read_csv(f'{y}/returns-ff5-mom.csv')  # read the returns
    for f in factors:
        print(f, y, 'portfolio regression')
        dat_f = dat[dat['factor'] == f].copy()
        portfolio = all_returns[store_columns].copy()  # create a dataframe to store the portfolio returns
        portfolio['coeff'] = f
        for w in weights:
            portfolio[w] = 0
            portfolio['weight_' + w] = 0
            for i, d in dat_f.iterrows():
                ticker = d['ticker']
                weight = d[f'weight_{w}']
                returns = all_returns[ticker]
                cum_returns = (1 + returns).cumprod()
                portfolio[w] += returns * weight
                portfolio['weight_' + w] += weight * cum_returns
        if not os.path.exists(f'{y}/{f}'):
            os.makedirs(f'{y}/{f}')
        portfolio.round(5).to_csv(f'{y}/{f}/portfolio{direction}.csv', index=False)
        # get unweighted portfolio
        for w in weights:
            portfolio[w] = portfolio[w] / portfolio[f'weight_{w}']
        portfolio['mkt-rf0'] = portfolio['mkt-rf']  # store the market returns
        portfolio['mkt-rf'] = portfolio['equal'] - portfolio['rf']  # change the market  to the equal weighted portfolio
        portfolio_output = pd.concat([portfolio_output, portfolio])  # store the portfolio output
        # Run regression to get the alpha for z-score portfolio
        x = sm.add_constant(portfolio[regression_factors])
        adj_returns = portfolio['z-score'] - all_returns['rf']
        model = sm.OLS(adj_returns, x).fit()
        # send model output to a txt file
        with open(f'{y}/{f}/model-output-z.txt', 'w') as j:
            j.write(model.summary().as_text())
        # get the total return
        total_return = (1 + portfolio['z-score']).prod() - 1
        # get coefficients and t-values
        alpha = model.params['const']
        alpha_t = model.tvalues['const']
        mkt_rf = model.params['mkt-rf']
        mkt_rf_t = model.tvalues['mkt-rf']
        smb = model.params['smb']
        smb_t = model.tvalues['smb']
        hml = model.params['hml']
        hml_t = model.tvalues['hml']
        rmw = model.params['rmw']
        rmw_t = model.tvalues['rmw']
        cma = model.params['cma']
        cma_t = model.tvalues['cma']
        mom = model.params['mom']
        mom_t = model.tvalues['mom']
        vol = adj_returns.std()
        # number of stocks in portfolio
        port = dat_f[dat_f['weight_z-score'] > 0]
        n = len(port)
        o = pd.DataFrame([{'year': y, 'factor': f, 'total_return': total_return, 'alpha': alpha, 'alpha_t': alpha_t,
                           'mkt-rf': mkt_rf, 'mkt_rf_t': mkt_rf_t, 'smb': smb, 'smb_t': smb_t, 'hml': hml,
                           'hml_t': hml_t, 'rmw': rmw, 'rmw_t': rmw_t, 'cma': cma, 'cma_t': cma_t, 'mom': mom,
                           'mom_t': mom_t,'vol':vol, 'value': 'z-score', 'n': n}])
        port.to_csv(f'{y}/{f}/portfolio.csv', index=False)
        year_data = pd.concat([year_data, o])
        # Run regression to get the alpha for t-value portfolio
        x = sm.add_constant(portfolio[regression_factors])
        adj_returns = portfolio['t-value'] - all_returns['rf']
        model = sm.OLS(adj_returns, x).fit()
        # send model output to a txt file
        with open(f'{y}/{f}/model-output-t.txt', 'w') as j:
            j.write(model.summary().as_text())
        # get the total return
        total_return = (1 + portfolio['t-value']).prod() - 1
        # get coefficients and t-values
        alpha = model.params['const']
        alpha_t = model.tvalues['const']
        mkt_rf = model.params['mkt-rf']
        mkt_rf_t = model.tvalues['mkt-rf']
        smb = 0
        smb_t = 0
        hml = 0
        hml_t = 0
        rmw = 0
        rmw_t = 0
        cma = 0
        cma_t = 0
        mom = 0
        mom_t = 0
        """smb = model.params['smb']
        smb_t = model.tvalues['smb']
        hml = model.params['hml']
        hml_t = model.tvalues['hml']
        rmw = model.params['rmw']
        rmw_t = model.tvalues['rmw']
        cma = model.params['cma']
        cma_t = model.tvalues['cma']
        mom = model.params['mom']
        mom_t = model.tvalues['mom']"""
        vol = adj_returns.std()
        o = pd.DataFrame([{'year': y, 'factor': f, 'total_return': total_return, 'alpha': alpha, 'alpha_t': alpha_t,
                           'mkt-rf': mkt_rf, 'mkt_rf_t': mkt_rf_t, 'smb': smb, 'smb_t': smb_t, 'hml': hml,
                           'hml_t': hml_t, 'rmw': rmw, 'rmw_t': rmw_t, 'cma': cma, 'cma_t': cma_t, 'mom': mom,
                           'mom_t': mom_t, 'vol': vol, 'value': 't-value'}])
        year_data = pd.concat([year_data, o])

# Send the data to a csv file
year_data.round(5).to_csv(f'year-data-full.csv', index=False)
portfolio_output.round(5).to_csv(f'portfolio-full-output.csv', index=False)

# do full regression for the portfolio
for f in factors:
    for w in ['z-score', 't-value', 'equal']:
        portfolio = portfolio_output[(portfolio_output['coeff'] == f)].copy()
        x = sm.add_constant(portfolio[regression_factors])
        adj_returns = portfolio[w] - portfolio['rf']
        model = sm.OLS(adj_returns, x).fit()
        # send model output to a txt file
        # create directory if it does not exist
        if not os.path.exists(f'full-portfolio/{f}'):
            os.makedirs(f'full-portfolio/{f}')
        with open(f'full-portfolio/{f}/model-output-{w}.txt', 'w') as j:
            j.write(model.summary().as_text())
        total_return = (1 + portfolio[w]).prod() - 1
        """alpha = model.params['const']
        alpha_t = model.tvalues['const']"""
