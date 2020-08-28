rbn <- function(n, dagobj, x) {
  if ( is.null( colnames(x) ) )  colnames(x) <-  paste("X", 1:dim(x)[2], sep = "")
  if ( !is.data.frame(x) )  x <- as.data.frame(x)
  bnlearn::rbn(x = dagobj, n = n, data = x)
}
