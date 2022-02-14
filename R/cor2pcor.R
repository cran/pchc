cor2pcor <- function(R){
  a <- solve(R)
  d <- dim(R)[1]
  com <- sqrt( diag(a) )
  for ( i in 1:(d-1) ) {
    for (j in i:d) {
      a[i, j] <- a[j, i] <- a[i, j]/ ( com[i] * com[j] )
    }
  }
  a <-  - a
  diag(a) <- 1
  a
}
