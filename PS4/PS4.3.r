# R
#### Stat243
#### Problem Set 4
#### 3. GLS estimator

rm(list = ls(all = TRUE)) # remove all objects

#### pseudo-code
# W = t(chol(S)) ### S = W %*% t(W)
# t(X) %*% S^(-1) %*% X %*% beta = t(X) %*% S^(-1) %*% Y
# t(X) %*% (W %*% t(W))^(-1) %*% X %*% beta = t(X) %*% (W %*% t(W))^(-1) %*% Y
# t(X) %*% t(W)^(-1) %*% W^(-1) %*% X %*% beta = t(X) %*% t(W)^(-1) %*% W^(-1) %*% Y
# t(X^*) %*% X^* %*% beta = t(X^*) %*% Y^*			#### X^* = W^(-1) %*% X;	Y^* = W^(-1) %*% Y
# Reduced to OLS:
# X^*.qr = qr(X^*); 
# Q = qr.Q(X^*.qr);  R = qr.R(X^*.qr) #### Q: orthogonal; R: upper triangular
# t(X^*) %*% X^* %*% beta = t(X^*) %*% Y^*
# t(R) %*% t(Q) %*% Q %*% R %*% beta = t(R) %*% t(Q) %*% Y
# R %*% beta = t(Q) %*% Y

#### test-code
n <- 2000
p <- 200
scale <- 2; #debug
n <- n/scale; p <- p/scale;

Z <- matrix(abs(rnorm(n^2)), n)
S <- crossprod(Z)/max(Z)
X <- matrix(rnorm(n*p)*10, n)
Y <- matrix(rnorm(n)*100, n)

gls <- function(X,S,Y){
	W <- t(chol(S)) ### S = W %*% t(W)
	X_s <- solve(W, X) #### X^* = W^(-1) %*% X
	Y_s <- solve(W, Y) #### Y^* = W^(-1) %*% Y
	X_s.qr <- qr(X_s)
	Q = qr.Q(X_s.qr)
	R = qr.R(X_s.qr)
	beta <- backsolve(R, crossprod(Q,Y))	
	return(beta)
}

beta_hat <- function(X,S,Y){
	Xt <- t(X)
	Sinv <- solve(S)
	beta <- solve(Xt %*% Sinv %*% X) %*% Xt %*% Sinv %*% Y
	return(beta)
}


benchmark(gls(X,S,Y), beta_hat(X,S,Y), replications = 5)[1:6]

