# R
#### Stat243
#### Problem Set 3
#### 1. mixtureMean

library(rbenchmark)

rm(list = ls(all = TRUE)) # remove all objects
load('mixtureMean.RData') # import data

# (a) original data storage
#mixmeanA <- sapply(1:length(IDsA), function (i){return(sum(muA[IDsA[[i]]]*wgtsA[[i]]))}) # sapply
#mixmeanA <- mapply(function(x, y) sum(muA[x]*y), IDsA, wgtsA) # mapply

# (b) data setup for A: K=1000
# table out as matrix storing the mu[ids]/wgts in cols for each observation row
idnum <- length(IDsA)
idlen <- sapply(1:idnum, function(i){return(length(IDsA[[i]]))})
maxmi <- max(idlen)

muidA <- matrix(as.numeric(NA), nr = idnum, nc = maxmi)
for (i in 1:idnum) {
	muidA[i, 1:idlen[i] ] <- muA[IDsA[[i]]]
}
wtidA <- matrix(as.numeric(NA), nr = idnum, nc = maxmi)
for (i in 1:idnum) {
	wtidA[i, 1:idlen[i] ] <- wgtsA[[i]]
}
#mixmeanA2 <- rowSums(muidA*wtidA, na.rm = TRUE)

# (c) data setup for B: K=10
# small K can allow us to store all the IDs as truth table for each u
idnum <- length(IDsB)
munum <- length(muB) # K is small

wtidB <- matrix(0, nr = munum, nc = idnum)
for (i in 1:idnum) {
	tmpwt <- rep(0, munum)
	tmpwt[ IDsB[[i]] ] <- wgtsB[[i]]
	wtidB[ ,i] <- tmpwt
}
#mixmeanB2 <- colSums(muB*wtidB)

# (d) efficiency comparison
benchmark(A1 = {mixmeanA<-sapply(1:length(IDsA), function(i){return(sum(muA[IDsA[[i]]]*wgtsA[[i]]))})},
          A2 = {mixmeanA2 <- rowSums(muidA*wtidA, na.rm = TRUE)}, replications = 5)
all.equal(mixmeanA, mixmeanA2)

benchmark(B1 = {mixmeanB<-sapply(1:length(IDsB), function(i){return(sum(muB[IDsB[[i]]]*wgtsB[[i]]))})},
          B2 = {mixmeanB2 <- colSums(muB*wtidB)}, replications = 5)
all.equal(mixmeanB, mixmeanB2)



