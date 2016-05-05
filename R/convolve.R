convolve <- function(a,K){
  # To retrieve the cols and rows
  a_row <- nrow(a)
  a_col <- ncol(a)
  b_row <- nrow(K)
  b_col <- nrow(K)
  
  
  # Rotate the matrix 180 deg cw, rotate twice
  h <- rotate(K)
  h <- rotate(h)
  
  # To locate the center, right, and left
  center1 <- floor((nrow(h) + 1)/2)
  center2 <- floor((ncol(h) + 1)/2)
  right <- b_col - center2
  left <- center2 - 1
  top <- center1 - 1
  bottom <- b_row - center2
  
  # To create a matrix of zeros
  row1 <- a_row+top+bottom
  row2 <- a_col+left+right
  rep <- matrix(rep(0,(row1*row2)),row1,row2)
  
  for (i in (1+top):(a_row+top)){
    for (j in (1+left):(a_col+left)){
      rep[i,j] <- a[i - top,j - left]
    }
  }

  result <- matrix(rep(0,(a_row*a_col)),
                   a_row,a_col)
  
  for (i in 1:a_row){
    for(j in 1:a_col){
      for(k in 1:b_row){
        for(l in 1:b_col){
          q <- i - 1
          w <- j - 1
          result[i,j] <- result[i,j] + 
            (rep[k+q,l+w]*h[k,l])
        }
      }
    }
  }
  
  return(result)
}