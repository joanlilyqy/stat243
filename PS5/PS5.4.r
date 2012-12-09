# R
#### Stat243
#### Problem Set 5
#### 4. EM

rm(list = ls(all = TRUE)) # remove all objects

theta <- rep(NA, 3)
names(theta) <- c("beta0", "beta1", "sigma2")

theta["beta0"]  <- 0.2
theta["beta1"]  <- 2
theta["sigma2"] <- 0.1

n <- 100
c <- n * 0.2 # test (a) exceedance=20%


x <- runif(n)
y <- rnorm(n, mean = theta["beta0"]+theta["beta1"]*x, sd = sqrt(theta["sigma2"]))

## (c) EM implementation