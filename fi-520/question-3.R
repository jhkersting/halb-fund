## Question 3
source("functions-3.R")
s = 210
rf = 0.042
vol = 0.25
x = 190
t = 1.2
option_type = "put"
div_y = 0.02

compound_type = 'put'
compound_x = 15
compound_t = 0.75
val <- compound_option(s, x, t, rf, vol, option_type, 300, div_y, compound_x, compound_t, compound_type)


# Find Delta  
h = 0.01
d1 <- compound_option(s - h, x, t, rf, vol, option_type, 300, div_y, compound_x, compound_t, compound_type)
d2 <- compound_option(s + h, x, t, rf, vol, option_type, 300, div_y, compound_x, compound_t, compound_type)
delta <- (d2 - d1) / (2 * h)


# Find Gamma
d1 <- compound_option(s - h, x, t, rf, vol, option_type, 300, div_y, compound_x, compound_t, compound_type)
d2 <- compound_option(s + h, x, t, rf, vol, option_type, 300, div_y, compound_x, compound_t, compound_type)
d3 <- compound_option(s, x, t, rf, vol, option_type, 300, div_y, compound_x, compound_t, compound_type)
gamma <- (d2 - 2 * d3 + d1) / h^2


# Find Vega
h = 0.0001
d1 <- compound_option(s, x, t, rf, vol - h, option_type, 300, div_y, compound_x, compound_t, compound_type)
d2 <- compound_option(s, x, t, rf, vol + h, option_type, 300, div_y, compound_x, compound_t, compound_type)
vega <- (d2 - d1) / (2 * h)



# Find new value of compound option
s_new = 220
vol_new = 0.27

# Use derivatives to find new value
delta_s <- s_new - s
delta_vol <- vol_new - vol
change <- delta_s * delta + delta_vol * vega + 0.5 * delta_s^2 * gamma
new_price <- val + change


# actual new value
new_val <- compound_option(s_new, x, t, rf, vol_new, option_type, 300, div_y, compound_x, compound_t, compound_type)
print(paste("Value of Compound: ", val))
print(paste("Delta: ", delta))
print(paste("Gamma: ", gamma))
print(paste("Vega: ", vega))
print(paste("New Value of Compound using Derivatives: ", new_price))
print(paste("Actual New Value of Compound: ", new_val))



