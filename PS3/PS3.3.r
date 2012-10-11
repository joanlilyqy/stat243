# R
#### Stat243
#### Problem Set 3
#### 3. Set: lower.triangle( matrix ) = transpose( upper.triangle( matrix ) ) 

rm(list=ls(all=TRUE)) # remove all objects

m <- matrix(1:25, nr=5) # simple test matrix
m #showcase

### (a)
m[lower.tri(m)] = t(m[upper.tri(m)]) # this code does not work
m #showcase
# m[upper.tri(m)] # convert to col-major vector (should be row-major conversion here)

### (b)
m <- matrix(1:25, nr=5) # simple test matrix
m #debug
m[lower.tri(m)] #debug, col-major
m[upper.tri(m)] #debug, col-major
### after examination, we have to get row-major upper tirangle
t(m) # transpose of orignal m
t(m)[lower.tri(m)] # col-major of t(m) = row-major of m

m #showcase
m[lower.tri(m)] <- t(m)[lower.tri(m)] # this works
m #showcase

### (c)
"lower.tri<-" <- function(x, byrow = FALSE, value){ #replacement function for lower.tri
	x <- as.matrix(x)
	if ( length(x[lower.tri(x)]) != length(value) ) stop("Vector length mismatch!")
	
	if (!byrow)	x[lower.tri(x)] <- value
	else {
		tmpx <- t(x)
		tmpx[upper.tri(tmpx)] <- value
		x <- t(tmpx)
	}
	return(x)
}

########test#########
m <- matrix(1:25, nr=5) # simple test matrix
m
lower.tri(m) <- c(1,2,3)
lower.tri(m) <- 1:10 ; 	m
lower.tri(m, byrow = TRUE) <- 1:10 ; m

