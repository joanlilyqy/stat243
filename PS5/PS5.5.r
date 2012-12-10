# R
#### Stat243
#### Problem Set 5
#### 5.

rm(list = ls(all = TRUE)) # remove all objects
source("ps5prob5.R")
data$logw <- log(data$wavelength) # log wavelength
data$verr <- (data$fluxerror)^2 # square fluxerror

## (a) plot flux data
attach(data)
par(mfrow = c(1,2),cex = 1, cex.main = 1)
plot(logw, flux, xlab = "log(wavelength)", ylab = "flux")
plot(time, flux, xlab = "time", ylab = "flux")

## (b) initialize
theta <- rep(NA, 7)
names(theta) <- c("kappa", "lambda", "sigma", "rou_w", "rou_t", "tau", "alpha")
# initial guess
theta["lambda"] <- diff(range(time)) / diff(range(meanpts$time))
theta["kappa"]  <- diff(range(flux)) / diff(range(meanpts$value))
theta["sigma"]  <- var(flux) * 0.8
theta["rou_w"]  <- max(diff(logw)) / 10
theta["rou_t"]  <- max(diff(time)) / 10
theta["tau"]    <- var(flux) * 0.1
theta["alpha"]  <- var(flux) * 0.1 / mean(verr)

## (c) optimize



