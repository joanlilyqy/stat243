# R
#### Stat243
#### Problem Set 3
#### 2. CSC matrix 

rm(list=ls(all=TRUE)) # remove all objects
source("cscFromC.R")

### (a)
makeCSCr <- function(matT){
	matCSC = list()
	dimInfo <- which(matT != 0, arr.ind = TRUE) # array indices info (row, col)
	matCSC$values <- matT[dimInfo] #non-zero matrix entries 
	matCSC$rowIndices <- dimInfo[ ,1] # row indices
	matCSC$colPointers <- c(1, cumsum(tabulate(dimInfo[ ,2], nbins = ncol(matT)))+1 ) # cumsum the num of entries in each col
  return(matCSC)
}

### (b) # more efficient after Rprof() and improvement in coding
makeCSCr2 <- function(matT){
	matCSC = list()
#	dimInfo <- which(matT != 0, arr.ind = TRUE) # array indices info (row, col)
	M <- as(matT, "dgCMatrix")
	matCSC$values <- M@x #non-zero matrix entries 
	matCSC$rowIndices <- M@i + 1 # row indices
	matCSC$colPointers <- M@p + 1 # cumsum the num of entries in each col
  return(matCSC)
}

############## Test ##################
#m <- makeTestMatrix(4)
#m <- makeTestMatrix(2500)
m <- makeTestMatrix(10000)
#m <- matrix(c(1,0,0,7,0,2,0,0,0,0,0,0,0,0,0,4), nr=4)
#m <- matrix(c(1,0,1,0,0,0,1,0,0), nr=3)
#m <- matrix(c(1,0,1,1,0,0,0,0,0), nr=3)

### (b)
system.time(makeCSC(m))
system.time(r <- makeCSCr(m))

Rprof("makeCSCr.prof", interval = 0.01)
system.time(r2 <- makeCSCr2(m))
summaryRprof("makeCSCr.prof")

### (c) byte compiling
library(compiler)
makeCSCrc <- cmpfun(makeCSCr)
makeCSCrc # notice the indication that the function is byte compiled.
system.time(r <- makeCSCr(m))
system.time(rc <- makeCSCrc(m))







