import pandas as pd


# combine every years portfolio data

years = list(range(2001,2023))
output = pd.DataFrame()
for y in years:
    portfolio = pd.read_csv(str(y) + '/mkt-rf-alpha/portfolio.csv')
    portfolio['weight'] = portfolio['weight_z-score']
    portfolio = portfolio[['year','ticker','weight','alpha','beta',]]
    portfolio.to_csv(str(y) + '/portfolio-clean.csv', index=False)
    output = pd.concat([output,portfolio])

output.round(4).to_csv('yearly-portfolio-weights.csv', index=False)


# clean portfolio returns
port = pd.read_csv('portfolio-full-output.csv')
port['return'] = port['z-score']

port = port[['date','return','mkt-rf']]

port.round(4).to_csv('daily-portfolio-returns.csv', index=False)
