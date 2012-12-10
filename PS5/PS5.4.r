# R
#### Stat243
#### Problem Set 5
#### 4. EM

rm(list = ls(all = TRUE)) # remove all objects

##############################################
#### test data generation
##############################################
set.seed(0)
n <- 100 # sample size
c <- n * 0.2 # test (a)/(b) exceedance=20%/80%
# true parameter values
theta <- rep(NA, 3)
names(theta) <- c("beta0", "beta1", "sigma2")
theta["beta0"]  <- 0.5
theta["beta1"]  <- 2
x <- runif(n)
theta["sigma2"] <- (theta["beta1"]/3)^2*sum( (x-mean(x))^2 )
# simulated data
e <- rnorm(n, mean = 0, sd = sqrt(theta["sigma2"]))
y <- theta["beta0"] + theta["beta1"]*x + e
sort.y <- sort(y, decreasing = TRUE)
tau <- sort.y[c+1] # threshold
y[which(y>tau)] <- as.numeric("NA")

#############################################
#### (c) EM implementation
#### input: x, y, tau
#############################################
EM.censor <- function(x,y,tau){
	obs <- which(!is.na(y)) # observed y indices
	cen <- which(is.na(y))  # censored y indices
	c <- length(cen)
	n <- length(y)  # sample size
	ny <- y #censored y's to be updated by EM
	
	# simulation setup 
	tol  <- 1e-8
	iter <- 0
	lim  <- 1000
	beta0.trace  <- matrix(NA, lim, 1)
	beta1.trace  <- matrix(NA, lim, 1)
	sigma2.trace <- matrix(NA, lim, 1)
	more = TRUE
	
	# initialization
	beta1  <- ( (n-c)*mean(y[obs]) + c*tau )/(n*mean(x))
	beta0  <- mean(y[obs]) - beta1*mean(x)
	sigma2 <- sum((y[obs]-beta0-beta1*x[obs])^2)/(n-c)
	
	# auxiliary function
	rho <- function(x){
	  return ( dnorm(x) / (1 - pnorm(x)) )
	}
	
	# main optimization
	while(more){
	  iter <- iter + 1
	  # E-step
	  mu <- beta0 + beta1*x
	  tauS <- (tau - mu)/sqrt(sigma2)
	  ny[cen] <- mu[cen] + sqrt(sigma2)*rho(tauS[cen]) # E(Z)
	  vy <- sigma2*(1 + tauS[cen]*rho(tauS[cen]) - rho(tauS[cen])^2) # V(Z)
	  # M-step
	  fit <- lm(ny~x) # LS fit for beta MLEs
	  b0 <- fit$coefficients[1]
	  b1 <- fit$coefficients[2]
	  sig2 <- (sum(fit$residuals^2) + sum(vy))/n # MLE of sigma2
	  # convergence
	  diff.th <- abs(beta0-b0) + abs(beta1-b1) + abs(log(sigma2)-log(sig2))
	  more <- (diff.th > tol) # check tol
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
	names(theta.result) <- c("beta0", "beta1", "sigma2")
	print(theta) #global true parameter
	print(theta.result)
	print(iter)
	# iterates results
	par(mfrow=c(3,1), mar=c(4,4,2,2),cex=1);
	plot(1:iter,beta0.trace[1:iter],xlab="iteration",ylab="beta0","l")
	plot(1:iter,beta1.trace[1:iter],xlab="iteration",ylab="beta1","l")
	plot(1:iter,sigma2.trace[1:iter],xlab="iteration",ylab="sigma2","l")
	
	par(mfrow=c(1,1));
	plot(x,y,ylim=c(min(y[obs])-1,max(y[obs])+2)) # plot data
	points(x[cen],ny[cen],type = "p",pch=20)
	lines(c(sort(x)[1],sort(x)[n]),c(beta0+beta1*sort(x)[1],beta0+beta1*sort(x)[n])) # LS line
}

EM.censor(x,y,tau)

#############################################
#### (d) BFGS implementation
#### input: x, y, tau
#############################################

BFGS.censor <- function(x,y,tau){
	obs <- which(!is.na(y)) # observed y indices
	cen <- which(is.na(y))  # censored y indices
	c <- length(cen)
	n <- length(y)  # sample size
	
	# initialization
	beta1  <- ( (n-c)*mean(y[obs]) + c*tau )/(n*mean(x))
	beta0  <- mean(y[obs]) - beta1*mean(x)
	sigma2 <- sum((y[obs]-beta0-beta1*x[obs])^2)/(n-c)
	init0 <- c(beta0,beta1,sigma2)
	
	# log liklihood
	pp.lik <- function(par, x, y, tau, obs, cen) {
	  b0   <- par[1]
	  b1   <- par[2]
	  sig2 <- par[3]
	  
		mu  <- b0 + b1*x
		if (sig2 <= 0)
			sig <- 1e-6
		else
			sig <- sqrt(sig2)
		
		obs.lik <- sum(log(dnorm(y[obs], mean = mu[obs], sd = sig)))
		cen.lik <- sum(log(pnorm(tau, mean = mu[cen], sd = sig, lower.tail=F)))
		
		return (-(obs.lik + cen.lik))
	}
	
	# optimize
	opt <- optim(init0, pp.lik, x = x, y = y, tau = tau, obs = obs, cen = cen, 
	method = 'BFGS', control = list(trace = TRUE, parscale = c(1,1,10)), hessian = TRUE) 
	# results
	print(theta)
	cat(opt$par,"\t theta \n")
	cat(sqrt(diag(solve(opt$hessian))), "\t se(theta) \n")
}
BFGS.censor(x,y,tau)

  
