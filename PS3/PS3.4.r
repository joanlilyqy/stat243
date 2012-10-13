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
x0 <- rep(0, p) # get length p vector
x0[1] <- 1 # initialize with starting from state 1
xm <- matrix(sample(rnd, p*p), nr = p) # get p-by-p matrix of rnds
xm <- xm / rowSums(xm) # normalize rows



##############################
#####     S3 method      #####
##############################

#########    (a)    ##########
### create the Markov S3 class
### initial state, transition matrix, num of steps, current state and [if needed, chain]
### vector, matrix, numeric, vector and [matrix]

runSteps <- function(object, ...) UseMethod("runSteps") # generic method
### creating a specific "runSteps" method for the MarkovS3 class construction
# runs a MarkovS3 obj n steps for construction
runSteps.MarkovS3 <- function(obj) { 
	# check validity
	if (class(obj) != 'MarkovS3') stop("Not valid on non-MarkovS3 objects!")
	# otherwise, we assume the P and init is already checked during construction

        obj$u <- obj$init
        if (obj$n == 0) {
          if (obj$stChain) obj$chain <- matrix(obj$u, nr = 1)
          return (obj)
          # no more steps, simple return the initial state
        } else {
          if (obj$stChain) {
            obj$chain <- matrix(as.numeric(NA), nr = obj$n + 1, nc = length(obj$init))
            obj$chain[1, ] <- obj$init
            for (i in 1:obj$n) obj$chain[i + 1, ] <- as.vector(obj$chain[i, ] %*% obj$P)
            obj$u <- obj$chain[obj$n + 1, ]
          } else {
            obj$u <- as.vector(obj$u %*% ( obj$P %^% obj$n))
            # directly propagate the Markov chain n steps, no intermediate steps stored
          }
          return(obj)
        }
}

# constructor for 'MarkovS3' class
MarkovS3 <- function(init = NA, P = NA, n = 0, u = NA, stChain = FALSE, chain = NA){	
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
	if (!all.equal(rowSums(P), rep(1, p)))
          stop("Transition probabilities for each state should sum to one!")
	
	# initialize the Markov chain
	obj <- list(init = init, P = P, n = n, u = NA, stChain = stChain, chain = NA)
	class(obj) <- 'MarkovS3'

        # construct the Markov chain
	obj <- runSteps(obj)
	return(obj)
}

mkc0 <- MarkovS3(x0, xm)
mkc0
mkc1 <- MarkovS3(x0, xm, n = 1)
mkc1
mkc2 <- MarkovS3(x0, xm, n = 2, stChain = TRUE)
mkc2

#########    (b)    ##########
#### Markov chain S3 class-specific operators

`+.MarkovS3` <- function(obj, incr) {
	# check validity
	if (incr < 0) stop("Need positive integer steps!")
        if (incr == 0) return (obj)
        
        obj$n <- obj$n + as.integer(incr) # add steps
        if (obj$stChain) {
            chain <- matrix(as.numeric(NA), nr = incr + 1, nc = length(obj$init))
            chain[1, ] <- obj$u # current state
            for (i in 1:incr) chain[i + 1, ] <- as.vector(chain[i, ] %*% obj$P)
            obj$u <- as.vector(chain[incr + 1, ])
            obj$chain <- rbind(obj$chain, chain[2:nrow(chain), ])
        } else {
            obj$u <- as.vector(obj$u %*% ( obj$P %^% incr))
            # directly propagate the Markov chain incr steps, no intermediate steps stored
        }
        return(obj)
}

`-.MarkovS3` <- function(obj, decr) {
	# check validity
	if (decr < 0) stop("Need positive integer steps!")
	if (obj$n < decr) stop("Not enough steps of states to remove!")
        if (decr == 0) return (obj)
	
	obj$n <- obj$n - as.integer(decr) # remove steps
	if (obj$stChain) {
            obj$u <- obj$chain[obj$n + 1, ]
            obj$chain <- obj$chain[1:(obj$n + 1), ]
            if (obj$n == 0) obj$chain <- matrix(obj$chain, nr = 1)
        } else {
            obj$u <- as.vector(solve(t(obj$P %^% decr), obj$u))
            # directly remove the Markov chain decr steps, no intermediate steps stored
        }
        return(obj)
}

`[.MarkovS3` <- function(obj, idVec) {
	# check validity
	ids = length(idVec); mx = as.integer(max(idVec)); 
	if (ids == 0) stop("No indices for extracting chain states!")
	if (length(idVec[idVec < 0 | idVec > obj$n])) stop("Indices out of bound!")	
	if (mx == 0) return(obj$init)

        if (obj$stChain) return(obj$chain[idVec + 1, ]) # 0 stands for the init state

	chain <- matrix(as.numeric(NA), nr = mx + 1, nc = length(obj$init))
	chain[1, ] <- obj$init
	for (i in 1:mx) chain[i + 1, ] <- as.vector(chain[i, ] %*% obj$P)
	return(chain[idVec + 1, ])
}

mkc0 <- MarkovS3(x0, xm)
mkc1 <- MarkovS3(x0, xm, 1)
mkc2 <- MarkovS3(x0, xm, 2)
mkc4 <- MarkovS3(x0, xm, 4)
all.equal(mkc1, mkc1 + 0)
all.equal(mkc1, mkc0 + 1)
all.equal(mkc4, mkc2 + 2)
all.equal(mkc4, mkc1 + 3) 
all.equal(mkc4, mkc0 + 4)

all.equal(mkc1, mkc4 - 3) 
all.equal(mkc2, mkc4 - 2) 
all.equal(mkc1, mkc2 - 1) 
all.equal(mkc4, mkc4 - 0)
all.equal(mkc0, mkc4 - 4)

mkc0[0]
mkc1[0:1]
mkc4[1:3]

mkc0 <- MarkovS3(x0, xm, stChain = TRUE)
mkc1 <- MarkovS3(x0, xm, 1, stChain = TRUE)
mkc2 <- MarkovS3(x0, xm, 2, stChain = TRUE)
mkc4 <- MarkovS3(x0, xm, 4, stChain = TRUE)
identical(mkc1, mkc1 + 0)
identical(mkc1, mkc0 + 1)
identical(mkc4, mkc2 + 2)
identical(mkc4, mkc1 + 3) 
identical(mkc4, mkc0 + 4)

identical(mkc1, mkc4 - 3) 
identical(mkc2, mkc4 - 2)
identical(mkc1, mkc2 - 1)
identical(mkc4, mkc4 - 0)
identical(mkc0, mkc4 - 4)

mkc0[0]
mkc1[0:1]
mkc4[1:3]

#########    (c)    ##########
#### Markov chain S3 class-specific functions
plot.MarkovS3 <- function(obj, ...) {
	if (obj$n == 0){
		cat("The current state is the same as the initial state\n")
		print(obj$u)
	}
	if (obj$n > 0) {
          if (obj$stChain) {
            chain <- obj$chain
          } else {
            chain <- matrix(as.numeric(NA), nr = obj$n + 1, nc = length(obj$init))
            chain[1, ] <- obj$init
            for (i in 1:obj$n) chain[i + 1, ] <- as.vector(chain[i, ] %*% obj$P)
          }
          matplot(1:(obj$n+1), chain, xlab="Steps", ylab="States Prob", ...)
	}
}

summary.MarkovS3 <- function(obj, ...) {
	cat("The initial state of the Markov chain is\n"); print(obj$init)
	cat("The transition matrix of this chain is\n"); print(obj$P)
	cat("The current step is", obj$n, ", and the current state is\n"); print(obj$u)
        if (obj$stChain) cat("The whole chain is stored.\n")

	#the empirical transition probabilities and empirical state probabilities (proportion of the time spent in each state).
}

print.MarkovS3 <- function(obj, ...) {
	cat("Initial state\n"); print(obj$init)
	cat("Transition matrix\n"); print(obj$P)
	cat("Current step\n"); print(obj$n)
	cat("Current state\n"); print(obj$u)
        if (obj$stChain) cat("The whole chain is stored.\n")
}


summary(mkc2)
print(mkc0)
show(mkc1)

plot(mkc0)
plot(mkc1)
mkclong <- MarkovS3(x0, xm, 10)
plot(mkclong)

