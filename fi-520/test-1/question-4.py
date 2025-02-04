import numpy as np
import functions as func

simulations = 100000
s = 40
vol = .4
rf = .045
t = 10
div = 0

# first option buying 0.4 calls at strike of 4
first_option = func.bsmovm(s, 4, t, rf, div, vol, 'call') * 0.4
print(f'First Option: {first_option}')
# second option writing 0.4 calls at strike of 10
second_option = -func.bsmovm(s, 10, t, rf, div, vol, 'call') * 0.4
print(f'Second Option: {second_option}')
# third option binary call at 30
third_option = -func.binary_option(s, 30, t, rf, div, vol) * 2.4
print(f'Third Option: {third_option}')
# value of security
value = first_option + second_option + third_option
print(f'Price of Security: {value}')



