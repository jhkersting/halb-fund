import pandas as pd


# combine every years portfolio data

years = list(range(2001,2023))
output = pd.DataFrame()
for y in years:
    portfolio = pd.read_csv(str(y) + '/mkt-rf-alpha/portfolio.csv')
    output = pd.concat([output,portfolio])

output.round(4).to_csv('yearly-portfolio-weights.csv', index=False)