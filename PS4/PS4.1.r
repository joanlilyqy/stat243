# R
#### Stat243
#### Problem Set 4
#### 1. add numbers of different sizes

rm(list = ls(all = TRUE)) # remove all objects
options(digits = 22)

# (a) eps=2.2e-16, so 16 decimal places

# (b) 
x <- c(1, rep(1e-15, 10000))
sum(x) # No, 10 decimal places

# (c)
sum = 0
for (i in 1:length(x))  sum = sum + x[i]
sum # No, 11 decimal places

sum = 0
for (i in 2:length(x))  sum = sum + x[i]
sum + x[1] # YES, 16 decimal places

# (d) 1e-16 repeat (b) and (c) 
x <- c(1, rep(1e-16, 10000))
sum(x) # (b) No, 11 decimal places

sum = 0
for (i in 1:length(x))  sum = sum + x[i]
sum # (c) No, 0 decimal places (1e-16 underflow double.eps)
sum = 0
for (i in 2:length(x))  sum = sum + x[i]
sum + x[1] # (c) YES, 16 decimal places

# (e) "sum" function, not just sum from left to right using regular double numbers

# (f) to get 4 decimal places, need 1e12 copies
1e12*8/1e9 #Gb

# (g)
rm(list = ls(all = TRUE)) # remove all objects
sum
# C function rsum()
# long double s = 0.0 # extended precision data type (80-bit, Sign+15Exp+1Int+63Frac)
# x86 Extended Precision Format (http://en.wikipedia.org/wiki/Extended_precision)
# for (i = 0; i < n; i=i+1) { s = s + x[i] }
# gives accurate results when reduced regular double precision







