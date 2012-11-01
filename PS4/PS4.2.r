# R
#### Stat243
#### Problem Set 4
#### 2. LU decomposition

library(rbenchmark)
rm(list = ls(all = TRUE)) # remove all objects

## (a)
######### LU Decomposition
# L_(n-1)%*%...%*%L_2%*%L_1%*%A = U # forward reduction deducted in class
# L = L_(n-1)^(-1)%*%...%*%L_2^(-1)%*%L_1^(-1) # no additional calculations
# b^* = L_(n-1)%*%...%*%L_2%*%L_1%*%b
#### For invertible A (n x n):
# FLOPS		:	rows*( col i + 2*col(i+1 to n) )  # zero-out ops are not calculated here
# -------------------------------------------
# L_1			: (n-1)*(0 + 2*(n-1))
# L_2			: (n-2)*(0 + 2*(n-2))
# ...
# L_(n-1)	: 1*(0 + 1)
# -------------------------------------------
# LU(A_nn) = sum_(i=1)^(n-1){(n-i)*(0+2*(n-i))} = 2n^3/3 - n^2 + n/3
# LU(b^*)  = sum_(i=1)^n{2*(i-1)} = n^2 - n

######### Chol Decomposition
# Positive definite (p.d.) A = t(U)%*%U
# b^* = t(U)^(-1)%*%b
#### For invertible A (n x n):
# FLOPS		: a_ii SQRT + col(i+1 to n) DIV + (i-1)*col(i, i+1 to n) MUL
# --------------------------------------------
# row 1		: 1 + (n-1) + 0
# row 2		: 1 + (n-2) + 1*(1 + n-2)
# row 3		: 1 + (n-3) + 2*(1 + n-3)
# ...
# row n-1	: 1 + 1 + (n-2)*(1 + 1)
# row n		: 1 + 0 + (n-1)*(1 + 0)
# --------------------------------------------
# Chol(A_nn) = sum_(i=1)^n{1 + (n-i) + (i-1)*(1 + n-i)} = n^3/6 + n^2/2 + n/3
# Chol(b^*)  = sum_(i=1)^n{i} = n^2/2 + n/2 ## backsolve(U, b, transpose = TRUE)

## (b)
# Backward elimination to solve Ux = b^*
# [equivalent to Chol b^*]
# LU(x) = Chol(x) = sum_(i=1)^n{i} = n^2/2 + n/2 ## backsolve(U, b^*)

## (c) A_nn decompositio stays the same, only b -> B so changes b^* to B^* (n x p)
# FUN(B^*) = p*FUN(b^*)
# FUN(X)   = p*FUN(x)

## (d) Explict inverse: V = A^(-1) using LU decomposition
# LUV = I
# (1) find L,U: see (a) LU
# (2) V = U^(-1)%*%(L^(-1)%*%I) = U^(-1)%*%(-L with diag(1)) ## backsolve(U, -L with diag(1))
# INV(A_nn) = LU(A_nn) + LU(X, p=n) = 7n^3/6 - n^2 + n/3
# INV(B^*)  = 0 ## B is unchanged, no B^* involved

## (e) matrix muliplication: X=V%*%B
# INV(X) = p*n^2

## (f) Total FLOPS: X = A^(-1)%*%B = FUN(A_nn) + FUN(B^*)
## (1) use LU: 				LU_O   = 2n^3/3 + (3p/2-1)n^2 + (1/3-p/2)n ~ O[(2n/3+3p/2)*n^2]
LU <- function(A, B){
	solve(A, B)
}
## (2) use Inverse		INV_O  = 7n^3/6 + (p-1)n^2 + n/3 ~ O[(7n/6+p)*n^2]
INV <- function(A, B){
	V <- solve(A)
	V %*% B
}
## (3) use Cholesky		Chol_O = n^3/6 + (p+1/2)n^2 + (1/3+p)n ~ O[(n/6+p)*n^2]
Chol <- function(A, B){
	U <- chol(A)
	backsolve(U, backsolve(U, B, transpose = TRUE))
}

## (g) empirical comparison: calculate X = A^(-1)%*%B
## test
n <- c(100, 3000) # really slow when n=3000, be careful
p <- c(1, 100, 3000)
for (i in 1:1){ #debug
	for (j in 1:3){
		nn <- n[i]
		pp <- p[j]
		# O[f(n)] of 3 methods:
		LU_O   <- 2*nn^3/3 + (3*pp/2-1)*nn^2 + (1/3-pp/2)*nn
		INV_O  <- 7*nn^3/6 + (pp-1)*nn^2 + nn/3
		Chol_O <- nn^3/6 + (pp+1/2)*nn^2 + (1/3+pp)*nn
		cat("n =",nn,"and p =",pp,":\n")
		cat("LU O[f(n)]:  ", format(LU_O,  width = 16,justify = 'right'),"\n");
		cat("INV O[f(n)]: ", format(INV_O, width = 16,justify = 'right'),"\n");
		cat("Chol O[f(n)]:", format(Chol_O,width = 16,justify = 'right'),"\n");

		Z <- matrix(rnorm(nn^2), nn)
		A <- crossprod(Z) #test matrix A
		B <- matrix(rnorm(nn*pp), nn) #test matrix B		
		cat("all.equal :",all.equal(LU(A,B), INV(A,B), Chol(A,B)), "\n")
		print(benchmark(LU(A,B), INV(A,B), Chol(A,B), replications = as.integer((2/i)^5))[1:6])
	}
}

## (h)

