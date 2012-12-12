# R
#### Stat243
#### Problem Set 5
#### 5. optim

rm(list = ls(all = TRUE)) # remove all objects
source("ps5prob5.R")

data$logw <- log(data$wavelength) # log wavelength
data$verr <- (data$fluxerror)^2 # square fluxerror

tt <- as.numeric(names(table(data$time))) # times

## (a) plot flux data
attach(data)
par(mfrow = c(1,2),cex = 1, cex.main = 1)
plot(logw, flux, xlab = "log(wavelength)", ylab = "flux")
plot(time, flux, xlab = "time", ylab = "flux")
detach(data)

## (b) initialize
theta <- rep(NA, 7)
names(theta) <- c("kappa", "lambda", "sigma2", "rho_w", "rho_t", "tau2", "alpha")
# initial guess
theta["lambda"] <- 0.8
theta["kappa"]  <- mean(data$flux) / mean(meanpts$value)

Ef <- meanfunc(theta, data$time)
resf <- data$flux - Ef
Covf <- outer(resf,resf)

theta["sigma2"] <- mean(diag(Covf)) * 0.3
theta["rho_w"]  <- diff(range(data$logw))
theta["rho_t"]  <- diff(range(data$time))
theta["tau2"]   <- mean(diag(Covf)) * 0.3
theta["alpha"]  <- mean(diag(Covf)) / mean(sqrt(data$verr))

init0 <- theta

## (c) optimize

covfunc <- with(list(verrs = data$verr),
function(theta, wavel, times){
  n <- length(times)
  CV <- matrix(0, n, n)
  scale_w <- wavel / theta["rho_w"]
  scale_t <- times / theta["rho_t"]
  alpv2 <- theta["alpha"] * verrs
  for (i in 1:n){
		for (j in 1:n){
		  del_w <- abs(scale_w[i] - scale_w[j])
		  del_t <- abs(scale_t[i] - scale_t[j])
		  cv <- theta["sigma2"] * exp(- del_w) * exp(- del_t)
		  if (!del_t){
		  	cv <- cv + theta["tau2"]
		  	if (!del_w){
		  		cv <- cv + alpv2[i]
		  	}
		  }
		  CV[i,j] <- cv
		}
	}
	CV
})

# negative log likelihood
require(mvtnorm)
nn.lik <- with(list(f = data$flux, w = data$logw, t = data$time),
function(par) {  
	MU <- meanfunc(par, t)
	CV <- covfunc(par, w, t)
	ll <- sum(sum(dmvnorm(f, MU, CV, log=T)))
	-ll
})

# optimize

init0 <- theta
opt1 <- optim(par = init0, fn = nn.lik, 
control = list(trace = TRUE, parscale = init0, reltol = 1e-6, maxit = 150), hessian = TRUE)
cat("theta \n", opt1$par, "\n")
cat("se(theta) \n", sqrt(diag(solve(opt1$hessian))), "\n")

init0 <- theta
opt2 <- optim(par = init0, fn = nn.lik, method = 'BFGS', 
control = list(trace = TRUE, parscale = init0, reltol = 1e-4, maxit = 20))
cat("theta \n", opt2$par, "\n")



