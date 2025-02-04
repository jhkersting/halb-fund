from nelson_siegel_svensson import NelsonSiegelSvenssonCurve as NSSC
import numpy as np
from nelson_siegel_svensson.calibrate import calibrate_ns_ols as cnols
from matplotlib import pyplot as plt
import pandas as pd

rates = pd.read_csv('treasury-rates.csv')
maturities = np.array(rates['mat'] /100)
yields = np.array(rates['yield'] /100)

curve, status = cnols(maturities, yields, tau0=1.0)  # starting value of 1.0 for the optimization of tau
assert status.success
t = np.linspace(0, 20, 100)
y = curve(t)

plt.plot(t, y)
plt.show()