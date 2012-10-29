# R
#### Stat243
#### Problem Set 4
#### 2. LU decomposition

library(rbenchmark)
rm(list = ls(all = TRUE)) # remove all objects

### calculate A^(-1)*B
## (1) use LU
LU <- function(A, B){
	solve(A, B)
}

## (2) use inverse
INV <- function(A, B){
	V <- solve(A)
	V %*% B
}

## (3) use Cholesky
Chol <- function(A, B){
	U <- chol(A)
	backsolve(U, backsolve(U, B, transpose = TRUE))
}

## (g) empirical comparison
n <- c(100, 3000)
p <- c(1, 100, 3000)

for (i in 1:2){
	for (j in 1:3){
		nn <- n[i]
		pp <- p[j]
		Z <- matrix(rnorm(nn^2), nn)
		A <- crossprod(Z) #test matrix A
		B <- matrix(rnorm(nn*pp), nn) #test matrix B
		
		cat("n =",nn,"and p =",pp,":\n")
		cat("all.equal :",all.equal(LU(A,B), INV(A,B), Chol(A,B)), "\n")
		print(benchmark(LU(A,B), INV(A,B), Chol(A,B), replications = as.integer((2/i)^5))[1:6])
	}
}


