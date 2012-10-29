# R
#### Stat243
#### Problem Set 4
#### 4. compare integer and floating point calculations
library(rbenchmark)

rm(list = ls(all = TRUE)) # remove all objects
#options(digits = 22)

### comparison: Integer vs. Double
xi <- rep(1:10, 100000)
xf <- rnorm(1000000)

benchmark(xi[100:10000], xf[100:10000], replications = 100)
benchmark(sum(xi), sum(xf), replications = 100)
benchmark(crossprod(xi), crossprod(xf), replications = 100)



