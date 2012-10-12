# R
#### Stat243
#### Problem Set 3
#### 4. Markov Chain (OOP)

library(methods)
library(expm)

rm(list = ls(all = TRUE)) # remove all objects
rnd <- rnorm(10000) # rnd sample
rnd <- rnd[rnd > 0] # prob > 0

p = 3 # num of states
x0 <- rep(0, p); x0[1] <- 1; # initialize with starting from state 1
xm <- matrix(sample(rnd, p*p), nr = p) # get p-by-p matrix of rnds
xm <- xm / rowSums(xm) # normalize rows


###### S3 method
mkcS3 <- list(init = NA, P = NA, n = 0, u = NA) # initial state, transition matrix, num of steps, current state
class(mkcS3) <- 'MarkovS3' # create the Markov S3 class

#########(a)
MarkovS3 <- function(init = NA, P = NA, n = 0, u = NA){	# constructor for 'MarkovS3' class
	# check validity
	p = length(init) # num of states
	nr = dim(P)[1]; nc = dim(P)[2]; # dim of transistion matrix
	
	if (n < 0) stop("Need positive integer steps!")
	
	if (p < 1) stop("Need at least one state for the Markov chain!")
	else if (p == 1) stop("Only one state, trival problem, no need for Markov chain.")
	
	if (nr != nc) stop("The transistion matrix has to be square!")
	if (p != nr) stop("Mismatch between state vector length and transition matrix dimension!")
	
	if (length(init[init < 0]) > 0) stop("All state probabilities should be positive!")
	if (length(P[P < 0]) > 0) stop("All transition probabilities should be positive!")
	
	if (sum(init) != 1) stop("State probabilities should sum to one!")
	if (!all.equal(rowSums(P), rep(1, p))) stop("Transition probabilities for each state should sum to one!")
	
	# propagate the Markov chain
	u <- init 
	if (n > 0) u <- u %*% ( P %^% n) # after n steps

	# assignment
	obj <- list(init = init, P = P, n = n, u = as.vector(u))
	class(obj) <- 'MarkovS3' 
	return(obj)
}

mkcS3 <- MarkovS3(x0, xm, 1)
mkcS3


# creating a generic method
runSteps <- function(object, ...) UseMethod("runSteps") 

# creating a specific method for the MarkovS3 class
runSteps.MarkovS3 <- function(obj, n = 1) { # return a MarkovS3 obj after n steps
	# check validity
	if (class(obj) != 'MarkovS3') stop("Not valid on non-MarkovS3 objects!")
	# otherwise, we assume the P and init is already checked during construction

	u <- obj$init
	if (n > 0) u <- u %*% ( obj$P %^% n) # propagate the Markov chain after n steps
	
	obj$n <- n
	obj$u <- as.vector(u)
	return(obj)
}

mkcS30 <- MarkovS3(x0, xm)
mkcS30
mkcS31 <- runSteps(mkcS30) # n = 1
identical(mkcS3, mkcS31)

##########(b)
# class-specific operators
`+.MarkovS3` <- function(obj, incr) {
	# check validity
	if (class(obj) != 'MarkovS3') stop("Not valid on non-MarkovS3 objects!")
	if (incr < 0) stop("Need positive integer steps!")
	
	if (incr > 0) obj$u <- obj$u %*% ( obj$P %^% incr) # after incr steps
	obj$n <- obj$n + as.integer(incr)
	obj$u <- as.vector(obj$u)
	return(obj)
}

`-.MarkovS3` <- function(obj, decr) {
	# check validity
	if (class(obj) != 'MarkovS3') stop("Not valid on non-MarkovS3 objects!")
	if (decr < 0) stop("Need positive integer steps!")
	if (obj$n < decr) stop("Not enough steps of states to remove!")	
	
	obj$n <- obj$n - as.integer(decr)
	# ????????????? how to improve
	u <- obj$init
	if (obj$n > 0) u <- u %*% ( obj$P %^% obj$n) # propagate the Markov chain after n steps
	obj$u <- as.vector(u)
	return(obj)
}

`[.MarkovS3` <- function(obj, idVec) {
	# check validity
	if (class(obj) != 'MarkovS3') stop("Not valid on non-MarkovS3 objects!")
	ids = length(idVec); mx = as.integer(max(idVec)); 
	if (ids < 1 || length(idVec[idVec < 0]) > 0) stop("Not valid indices")	
	if (mx == 0) return(obj$init)

	umat <- matrix(as.numeric(NA), nr = mx, nc = length(obj$init))
	Pseq <- obj$init
	for (i in 1:mx){
		Pseq <- Pseq %*% obj$P
		umat[i, ] <- as.vector(Pseq)
	}
	return(umat[idVec, ])
}


mkcS32 <- MarkovS3(x0, xm, 2)
identical(mkcS31, mkcS30 + 1)
identical(mkcS32, mkcS30 + 2)

identical(mkcS31, mkcS32 - 1)
identical(mkcS30, mkcS32 - 2)
#identical(mkcS30, mkcS32 - 3)

mkcS32[0]
mkcS32[1:2]


############(c)
plot.MarkovS3 <- function(obj, ...) {
	# check validity
	if (class(obj) != 'MarkovS3') stop("Not valid on non-MarkovS3 objects!")
	
	if (obj$n == 0){
		cat("The current state is the same as the initial state\n")
		print(obj$u)
	}
	if (obj$n > 0) {
		umat <- matrix(as.numeric(NA), nr = obj$n + 1, nc = length(obj$init))
		Pseq <- obj$init
		umat[1, ] <- obj$init
		for (i in 1:obj$n){
			Pseq <- Pseq %*% obj$P
			umat[i+1, ] <- as.vector(Pseq)
		}
		matplot(1:(obj$n+1), umat, xlab="Steps", ylab="States Prob", ...)
	}
}

summary.MarkovS3 <- function(obj, ...) {
	# check validity
	if (class(obj) != 'MarkovS3') stop("Not valid on non-MarkovS3 objects!")
	
	cat("The initial state of the Markov chain is\n"); print(obj$init)
	cat("The transition matrix of this chain is\n"); print(obj$P)
	cat("The current step is", obj$n, ", and the current state is\n"); print(obj$u)
	
	#the empirical transition probabilities and empirical state probabilities (proportion of the time spent in each state).
}

print.MarkovS3 <- function(obj, ...) {
	# check validity
	if (class(obj) != 'MarkovS3') stop("Not valid on non-MarkovS3 objects!")
	
	cat("Initial state\n"); print(obj$init)
	cat("Transition matrix\n"); print(obj$P)
	cat("Current step\n"); print(obj$n)
	cat("Current state\n"); print(obj$u)
}


summary(mkcS32)
print(mkcS30)
show(mkcS31)

plot(mkcS30)
plot(mkcS31)
mkcS3long <- MarkovS3(x, xm, 10)
plot(mkcS3long)

