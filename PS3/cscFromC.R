makeCSC = function(mat){
  matCSC = list()

  ncols = ncol(mat);
  matCSC$colPointers = integer(ncols + 1L);
  count.total = 1L;
  matCSC$colPointers[1] = 1L;
  for (j in 1:ncols)
    {
      nz = which(mat[, j] != 0);
      count.total = count.total + length(nz);
      matCSC$colPointers[j + 1] = count.total;
    }
  
  matCSC$values = numeric(count.total - 1);
  matCSC$rowIndices = integer(count.total - 1);
  index = 0L;
  for (j in 1:ncols)
    {
      nz = which(mat[, j] != 0);
      lnz = length(nz);
      if(lnz){
        matCSC$values[index + 1:lnz] = mat[nz, j];
        matCSC$rowIndices[index + 1:lnz] = nz;
      }
      index = index + lnz;
    }
  
  return(matCSC)
}
			

makeTestMatrix <- function(size){
  m1 <- sqrt(size)
  if(m1%%1 != 0) stop("size argument should be the square of an integer")
  x <- y <- seq(0, 1, len = m1)
  xy <- expand.grid(x,y)
  
  id <- 1:size
  mat <- matrix(0L, nr = size, nc = size)
  
  tmp <- cbind(id, id + 1)
  tmp <- tmp[!(tmp[,1]%%m1 == 0), ]
  tmp <- tmp[tmp[ , 2] <= size, ]
  mat[tmp] <- -1L
  
  tmp <- cbind(id, id - 1)
  tmp <- tmp[!((tmp[,1]-1)%%m1  == 0), ]
  tmp <- tmp[tmp[ , 2] >= 1, ]
  mat[tmp] <- -1L
  
  tmp <- cbind(id, id + m1)
  tmp <- tmp[tmp[ , 2] <= size, ]
  mat[tmp] <- -1L
  
  tmp <- cbind(id, id - m1)
  tmp <- tmp[tmp[ , 2] >= 1, ]
  mat[tmp] <- -1L
  
  diag(mat) <- -as.integer(rowSums(mat))
  return(mat)
}
