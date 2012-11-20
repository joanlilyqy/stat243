# R
#### Stat243
#### Problem Set 5
#### 3. Importance sampling

rm(list = ls(all = TRUE)) # remove all objects

## (a)

# we want to estimate EX and E(X^2) for a standrad normal
# let's draw from a t with 3 degrees of freedom

m <- 10000 # number of samples for each estimator
# standard MC estimator
set.seed(0)
y <- rnorm(m)
stdEX <- mean(y)
stdVX <- mean(y^2)

# samples for importance sampler
set.seed(0)
x <- rt(m, df = 3)  # i.e sample from g(x) being a t with df=3
f <- dnorm(x)  # density of x under f
g <- dt(x, df = 3)  # density of x under g
w <- f/g  # weights
isEX <- mean(x*w)
isVX <- mean((x*w)^2)

par(mfrow=c(2,1),cex= 0.5, cex.main= 1)
hist(w)
hist(x*w)

## (b)

# we want to estimate EX and E(X^2) for a t with v=3
# let's draw from a standard normal

m <- 10000 # number of samples for each estimator
# standard MC estimator
set.seed(0)
y <- rt(m, df = 3)
stdEX <- mean(y)
stdVX <- mean(y^2)

# samples for importance sampler
set.seed(0)
x <- rnorm(m)  # i.e sample from g(x) being a standard normal
f <- dt(x, df = 3)  # density of x under f
g <- dnorm(x)  # density of x under g
w <- f/g  # weights
isEX <- mean(x*w)
isVX <- mean((x*w)^2)

par(mfrow=c(2,1),cex= 0.5, cex.main= 1)
hist(w)
hist(x*w)

