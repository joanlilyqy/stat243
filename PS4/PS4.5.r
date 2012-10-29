# R
#### Stat243
#### Problem Set 4
#### 5. Condition number of eigen decomposition

rm(list = ls(all = TRUE)) # remove all objects

### aux function
norm2 <- function(x) sqrt(sum(x^2))

### Create a set of eigenvectors
n <- 100
Z <- matrix(rnorm(n^2), n)
A <- crossprod(Z)
GAMMA <- eigen(A)$vec #eigen vectors

### Explore positive eigenvalues of different magnitudes
t <- 52 # computer presicion bits
errors <- rep(as.numeric(NA), t)
magval <- rep(as.numeric(NA), t)
condno <- rep(as.numeric(NA), t)

for (i in 1:t){
	ids <- sample(1:n, n/4) # get a random set of indices to "enlarge" with condition number
	if (i == 1)	lambda <- rep(mean(abs(rnorm(n))), n) # i=1, with all eigen values equal
	else lambda <- abs(rnorm(n))
	lambda[ids] <- lambda[ids]*(2^(i-1)) # generate eigen values with certain condition number
	magval[i] <- max(lambda)
	condno[i] <- magval[i]/min(lambda)
	
	LAMBDA <- diag(lambda)
	testA <- GAMMA %*% LAMBDA %*% t(GAMMA) # create p.d. matrix
	errors[i] <- norm2(eigen(testA)$val - lambda)/norm2(lambda)
	
	if (length(which(eigen(testA)$val < 0)) > 0){
		cat("Failed at condition number", condno[i], "with relative error of", errors[i], "\n")
		par(mfrow=c(2,1),cex= 0.5, cex.main= 1)
		plot(log10(condno), errors, xlim=c(0, 17), ylim=c(0, 1.6), 
		     main="Relative error in estimated eigenvalues vs. condition number")
		plot(log10(magval), errors, xlim=c(0, 17), ylim=c(0, 1.6), 
		     main="Relative error in estimated eigenvalues vs. magnitude of eigenvalues")
		break
	}
}
