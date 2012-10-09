# R
#### Stat243
#### Problem Set 3
#### 2. CSC matrix 

rm(list=ls(all=TRUE)) # remove all objects
source("cscFromC.R")

### (a)
makeCSCr <- function(matT){
	matCSC = list()
	matCSC$values <- matT[matT != 0] #non-zero matrix entries
	dimInfo <- which(matT != 0, arr.ind = TRUE) # array indices info (row, col)
	matCSC$rowIndices <- dimInfo[ ,1] # row indices
	matCSC$colPointers <- c(1, cumsum(tabulate(dimInfo[ ,2], nbins = ncol(matT)))+1 ) # cumsum the num of entries in each col
  return(matCSC)
}

### (b)
makeCSCr2 <- function(matT){ # more efficient after Rprof() and improvement in coding
	matCSC = list()
	dimInfo <- which(matT != 0, arr.ind = TRUE) # array indices info (row, col)
	matCSC$values <- matT[dimInfo] #non-zero matrix entries 
	matCSC$rowIndices <- dimInfo[,1] # row indices
	matCSC$colPointers <- c(1, cumsum(tabulate(dimInfo[, 2], nbins = ncol(matT)))+1 ) # cumsum the num of entries in each col
  return(matCSC)
}


############## Test ##################
m <- makeTestMatrix(4)
m <- makeTestMatrix(10000)
#m <- matrix(c(1,0,0,7,0,2,0,0,0,0,0,0,0,0,0,4), nr=4)
#m <- matrix(c(1,0,1,0,0,0,1,0,0), nr=3)
#m <- matrix(c(1,0,1,1,0,0,0,0,0), nr=3)

Rprof("makeCSCr.prof")
makeCSCr2(m)
summaryRprof("makeCSCr.prof")

#system.time(makeCSC(m))
#system.time(makeCSCr(m))
system.time(makeCSCr2(m))






