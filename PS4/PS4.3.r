# R
#### Stat243
#### Problem Set 4
#### 3. GLS estimator

rm(list = ls(all = TRUE)) # remove all objects

n <- 2000
p <- 200

Z <- matrix(abs(rnorm(n^2)), n)
SIGMA <- crossprod(Z)/max(Z)
X <- matrix(rnorm(n*p)*10, n)
Y <- rnorm(p)*100

gls <- function(X,S,Y){
	beta <- 0
	return(beta)
}
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

function (formula, data, W) 
{
    Y <- model.response(m)
    X <- model.matrix(Terms, m, contrasts)
    n <- nrow(X)
    if (any(dim(W) != c(n, n))) 
        stop("dim(W) is not correct")
    eW <- eigen(W, TRUE)
    d <- eW$values
    if (any(d <= 0)) 
        stop("'W' is not positive definite")
    A <- diag(d^(-0.5)) %*% t(eW$vector)
    Ainv <- eW$vector %*% diag(d^(0.5))
    fit <- lm.fit(A %*% X, A %*% Y, method = method, ...)

}


