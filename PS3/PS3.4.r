# R
#### Stat243
#### Problem Set 3
#### 4. Markov Chain (OOP)

library(methods)
library(expm)

rm(list = ls(all = TRUE)) # remove all objects
rnd <- rnorm(50000) # rnd sample
rnd <- rnd[rnd > 0] # prob > 0

p = 5 # num of states
x0 <- rep(0, p) # get length p vector
x0[1] <- 1 # initialize with starting from state 1
xm <- matrix(sample(rnd, p*p), nr = p) # get p-by-p matrix of rnds
xm <- xm / rowSums(xm) # normalize rows

drawState <- function (p, pr) {
    if (length(pr) != p) stop("Vector length mismatch!")
    states <- diag(nrow = p, ncol = p) # state vectors
    id <- sample(1:5, 1, prob = pr)
    return (as.vector(states[id, ]))
}



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
    if (obj$n == 0) {
        obj$chain <- matrix(obj$init, nr = 1)
        return (obj)  # no more steps, simple return the initial state
    } else {
        p = length(obj$init)
        obj$chain <- matrix(as.numeric(NA), nr = obj$n + 1, nc = p)
        obj$chain[1, ] <- obj$init
        for (i in 1:obj$n)
            obj$chain[i + 1, ] <- drawState(p, as.vector(obj$chain[i, ] %*% obj$P))
    }
    return(obj)
}

# constructor for 'MarkovS3' class
MarkovS3 <- function(init = NA, P = NA, n = 0, chain = NA){	
    # check validity
    p = length(init) # num of states
    nr = dim(P)[1]; nc = dim(P)[2]; # dim of transition matrix
	
    if (n < 0) 
        stop("Need positive integer steps!")
    if (p < 2)
        stop("Need at least two states for the Markov chain!")
    if (nr != nc) 
        stop("The transistion matrix has to be square!")
    if (p != nr)
        stop("Mismatch between state vector length and transition matrix dimension!")
    if (length(init[init < 0]) > 0) 
        stop("All state probabilities should be positive!")
    if (length(P[P < 0]) > 0) 
        stop("All transition probabilities should be positive!")
    if (sum(init) != 1) 
        stop("State probabilities should sum to one!")
    if (!all.equal(rowSums(P), rep(1, p)))
        stop("Transition probabilities for each state should sum to one!")

    # initialize the Markov chain
    obj <- list(init = init, P = P, n = n, chain = NA)
    class(obj) <- 'MarkovS3'

    # construct the Markov chain
    obj <- runSteps(obj)
    return(obj)
}

mkc0 <- MarkovS3(x0, xm)
mkc0
mkc1 <- MarkovS3(x0, xm, n = 1)
mkc1
mkc2 <- MarkovS3(x0, xm, n = 1000)
mkc2

#########    (b)    ##########
#### Markov chain S3 class-specific operators

`+.MarkovS3` <- function(obj, incr) {
    # check validity
    if (incr < 0) stop("Need positive integer steps!")
    if (incr == 0) return (obj)
        
    p = length(obj$init)
    addchain <- matrix(as.numeric(NA), nr = incr + 1, nc = p)
    addchain[1, ] <- obj$chain[obj$n + 1, ] # current state
    for (i in 1:incr) 
        addchain[i + 1, ] <- drawState(p, as.vector(addchain[i, ] %*% obj$P))
    obj$n <- obj$n + as.integer(incr) # add steps
    obj$chain <- rbind(obj$chain, addchain[2:(incr+1), ])
    return(obj)
}

`-.MarkovS3` <- function(obj, decr) {
    # check validity
    if (decr < 0) stop("Need positive integer steps!")
    if (obj$n < decr) stop("Not enough steps of states to remove!")
    if (decr == 0) return (obj)
	
    obj$n <- obj$n - as.integer(decr) # remove steps
    obj$chain <- obj$chain[1:(obj$n + 1), ]
    if (obj$n == 0) 
        obj$chain <- matrix(obj$chain, nr = 1)
    return(obj)
}

`[.MarkovS3` <- function(obj, idVec) {
    # check validity
    if (length(idVec) == 0) stop("No indices for extracting chain states!")
    if (length(idVec[idVec < 1 | idVec > obj$n+1])) stop("Indices out of bound!")	

    return(obj$chain[idVec, ]) # init state is 1st
}
# show only the chain
mkc1$chain
(mkc1 + 1)$chain
(mkc1 - 1)$chain
mkc2[10:15]


#########    (c)    ##########
#### Markov chain S3 class-specific functions
plot.MarkovS3 <- function(obj, ...) {
    if (obj$n == 0)
        cat("The current state is the same as the initial state\n", obj$chain[1, ], "\n")
    if (obj$n > 0){
        data <- which(obj$chain > 0, arr.ind = TRUE)
        plot(data[ ,1], data[ ,2], xlab="Steps", ylab="States Prob", ...)
        hist(data[ ,2], c(0:5+0.5), probability = TRUE, main = "Histogram of states", xlab = "Markov chain states")
    }
}

summary.MarkovS3 <- function(obj, ...) {
    p <- length(obj$init)
    allPre <- colSums(obj$chain[-(obj$n+1), ])
    Tsub <- obj$chain[-1, ] - obj$chain[-(obj$n+1), ] # transition records (1: from, -1: to)
    Tadd <- obj$chain[-1, ] + obj$chain[-(obj$n+1), ]; Tadd[which(Tadd == 1)] <- 0; # retain the state
    Pemp <- diag(colSums(Tadd)/2/allPre, nrow = p, ncol = p) # state vectors
    for (i in 1:p)
        for (j in 1:p)
            if (i != j) Pemp[i,j] <- length(which(Tsub[ ,i] == 1 & Tsub[ ,j] == -1))/allPre[i]
        
    cat("The initial state of the Markov chain is\n"); print(obj$init)
    cat("The transition matrix of this chain is\n"); print(obj$P)
    cat("The current step is", obj$n, ", and the current state is\n"); print(obj$chain[obj$n+1, ])
    cat("The empirical transition probabilities are \n"); print(Pemp)
    cat("The empirical state probabilities are \n"); print(colMeans(obj$chain))
}

print.MarkovS3 <- function(obj, ...) {
    cat("Initial state\n"); print(obj$init)
    cat("Transition matrix\n"); print(obj$P)
    cat("Current step\n"); print(obj$n)
    cat("Current state\n"); print(obj$chain[obj$n+1, ])
}


summary(mkc2)
print(mkc1)

plot(mkc0)
plot(mkc2)

