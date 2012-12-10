# R
#### Stat243
#### Problem Set 5
#### 4. EM

rm(list = ls(all = TRUE)) # remove all objects

##############################################
#### test data generation
##############################################
n <- 100 # sample size
c <- n * 0.8 # test (b) exceedance=80%
# true parameter values
theta <- rep(NA, 3)
names(theta) <- c("beta0", "beta1", "sigma2")
theta["beta0"]  <- 0.5
theta["beta1"]  <- 2

# simulated data
x <- runif(n)
sxx <- sqrt( sum( (x-mean(x))^2 ) )
theta["sigma2"] <- (theta["beta1"]*sxx/3)^2 

y <- rnorm(n, mean = theta["beta0"] + theta["beta1"]*x, sd = sqrt(theta["sigma2"]))
sort.y <- sort(y, decreasing = TRUE)
tau <- sort.y[c+1] # threshold
y[which(y>tau)] <- as.numeric("NA")

#############################################
#### (c) EM implementation
#### input: x, y, tau
#############################################
obs <- which(!is.na(y)) # observed y indices
cen <- which(is.na(y))  # censored y indices
c <- length(cen)
n <- length(y)  # sample size
mt <- y # censored y's to be updated by EM 

# simulation setup 
tol  <- 1e-9
iter <- 0
lim  <- 1000
beta0.trace  <- matrix(NA, lim, 1)
beta1.trace  <- matrix(NA, lim, 1)
sigma2.trace <- matrix(NA, lim, 1)
more = TRUE

# initialization
beta1  <- ( sum(y[obs]) + c*tau )/sum(x)
beta0  <- ( sum(y[obs]) - beta1*sum(x[obs]) )/(n-c)
sigma2 <- ( sum((y[obs]-beta0-beta1*x[obs])^2) + sum((tau-beta0-beta1*x[cen])^2) )/n

# auxiliary function
rho <- function(x){
  return ( dnorm(x) / (1-pnorm(x)) )
}

# main optimization
while(more){
  iter <- iter + 1
  # E-step
  mu <- beta0 + beta1*x
  mt <- y
  
  tauS <- (tau - mu)/sqrt(sigma2)
  mt[cen] <- mu[cen] + sqrt(sigma2)*rho(tauS[cen]) # E(Z)
  vt <- sigma2*(1 + tauS[cen]*rho(tauS[cen]) + rho(tauS[cen])^2) # V(Z)
  # M-step
  fit <- lm(mt~x) # LS fit for beta MLEs
  b0 <- fit$coefficients[1]
  b1 <- fit$coefficients[2]
  sig2 <- (sum(fit$residuals^2) + sum(vt))/n
  # convergence
  diff <- abs(beta0-b0) + abs(beta1-b1) + abs(log(sigma2)-log(sig2))
  more <- (diff > tol) # check tol
  # update theta
  beta0 <- b0
  beta1 <- b1
  sigma2 <- sig2
  # record trace
  beta0.trace[iter]  <- beta0
  beta1.trace[iter]  <- beta1
  sigma2.trace[iter] <- sigma2
}

theta.result <- c(beta0,beta1,sigma2)

#############################################
#### (d) BFGS implementation
#############################################






  
