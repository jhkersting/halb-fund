import numpy as np
import pandas as pd
import functions as func


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

