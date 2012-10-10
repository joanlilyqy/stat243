# R
#### Stat243
#### Problem Set 3
#### 2. CSC matrix 

rm(list=ls(all=TRUE)) # remove all objects
source("cscFromC.R")

### (a) R version of makeCSC
makeCSCr <- function(matT){
	matCSC = list()
	dimInfo <- which(matT != 0, arr.ind = TRUE) # array indices info (row, col)
	matCSC$values <- matT[dimInfo] #non-zero matrix entries 
	matCSC$rowIndices <- dimInfo[ ,1] # row indices
	matCSC$colPointers <- c(1, cumsum(tabulate(dimInfo[ ,2], nbins = ncol(matT)))+1 ) # cumsum the num of entries in each col
  return(matCSC)
}

### (b) # more efficient after Rprof() and improvement in coding
library(Matrix)
makeCSCr2 <- function(matT){
	matCSC = list()
	M <- as(matT, "dgCMatrix")
	matCSC$values <- M@x #non-zero matrix entries 
	matCSC$rowIndices <- M@i + 1 # row indices
	matCSC$colPointers <- M@p + 1 # cumsum the num of entries in each col
  return(matCSC)
}

###### compare the CSC representation of matrix
compMat <- function(m1, m2){ 
	flag = all.equal(m1$values, m2$values)
	flag = flag && all.equal(m1$rowIndices, m2$rowIndices)
	flag = flag && all.equal(m1$colPointers, m2$colPointers)
	return(flag)
}


############## Test ##################
### (d) memory usage
gc()

#m <- makeTestMatrix(4)
#m <- makeTestMatrix(2500)
m <- makeTestMatrix(10000)
#m <- matrix(c(1,0,0,7,0,2,0,0,0,0,0,0,0,0,0,4), nr=4)
#m <- matrix(c(1,0,1,0,0,0,1,0,0), nr=3)
#m <- matrix(c(1,0,1,1,0,0,0,0,0), nr=3)

gc()
system.time(mr <- makeCSCr(m))
gc()

### (b) profiling for performance improvement
system.time(mc <- makeCSC(m))
compMat(mc, mr)
gc()

Rprof("makeCSCr.prof", interval = 0.01)
system.time(mr2 <- makeCSCr2(m))
Rprof(NULL)
summaryRprof("makeCSCr.prof")
compMat(mc, mr2)
gc()

### (c) byte compiling
library(compiler)
makeCSCrCMP <- cmpfun(makeCSCr)
makeCSCrCMP # notice the indication that the function is byte compiled.
system.time(mrcmp <- makeCSCrCMP(m))
compMat(mc, mrcmp)
gc()







