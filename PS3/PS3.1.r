# R
#### Stat243
#### Problem Set 3
#### 1. mixtureMean

rm(list=ls(all=TRUE)) # remove all objects
load('mixtureMean.RData') # import data

# (a) original data storage
#mixmeanA <- sapply(1:length(IDsA), function (i) { return( sum(muA[IDsA[[i]]]*wgtsA[[i]]) ) } ) # sapply, for-loop style
#mixmeanA <- mapply(function(x, y) sum(muA[x]*y), IDsA, wgtsA) # mapply

# (b) data setup for A: K=1000
maxmi <-  max(sapply(1:length(wgtsA), function(i){return(length(wgtsA[[i]]))}))
muidA <- matrix(as.numeric(NA), length(IDsA), maxmi)
for (i in 1:length(IDsA)) {
	muidA[i, 1:length(IDsA[[i]]) ] <- muA[IDsA[[i]]]
}
wtidA <- matrix(as.numeric(NA), length(IDsA), maxmi)
for (i in 1:length(IDsA)) {
	wtidA[i, 1:length(wgtsA[[i]]) ] <- wgtsA[[i]]
}
#mixmeanA2 <- rowSums(muidA*wtidA, na.rm = TRUE)

# (c) data setup for B: K=10
wtidB <- matrix(0, length(muB), length(IDsB))
for (i in 1:length(IDsB)) {
	tmpwt <- rep(0, length(muB))
	tmpwt[ IDsB[[i]] ] <- wgtsB[[i]]
	wtidB[ ,i] <- tmpwt
}
mixmeanB2 <- colSums(muB*wtidB, na.rm = TRUE)



# (d)
system.time(mixmeanA <- mapply(function(x, y) sum(muA[x]*y), IDsA, wgtsA))
system.time(mixmeanA2 <- rowSums(muidA*wtidA, na.rm = TRUE))
all.equal(mixmeanA, mixmeanA2)

system.time(mixmeanB <- mapply(function(x, y) sum(muB[x]*y), IDsB, wgtsB))
system.time(mixmeanB2 <- colSums(muB*wtidB))
all.equal(mixmeanB, mixmeanB2)



