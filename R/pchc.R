pchc <- function(x, method = "pearson", alpha = 0.05,
                 ini.stat = NULL, ini.pvalue = NULL, restart = 10, score = "bge", blacklist = NULL, whitelist = NULL) {

  runtime <- proc.time()
  if ( method == "cat"  &  !is.matrix(x) )  {
    for ( i in 1:dim(x)[2] ) x[, i] <- as.numeric(x[, i]) - 1
    x <- as.matrix(x)
  }
  ## score for continuous : "bge" (default), "loglik-g", "aic-g", "bic-g", "bge"
  ## score for discrete : "bde", "loglik", "bic"
  a <- Rfast::pc.skel(x, method = method, alpha = alpha, stat = ini.stat, ini.pvalue = ini.pvalue)
  nama <- colnames(x)
  if ( is.null(nama) )  nama <-  paste("X", 1:dim(x)[2], sep = "")
  colnames(x) <- nama
  vale <- which(a$G == 1)
  dag <- NULL
  score <- NULL
  if ( length(vale) > 0 ) {
    x <- as.data.frame(x)
    mhvale <- as.data.frame( which(a$G == 0, arr.ind = TRUE) )
    mhvale[, 1] <- nama[ mhvale[, 1] ]
    mhvale[, 2] <- nama[ mhvale[, 2] ]
    colnames(mhvale) <- c("from", "to")
    if ( !is.null(blacklist) ) {
      colnames(blacklist) <- c("from", "to")
      mhvale <- rbind(mhvale, blacklist)
    }
    if ( !is.null(whitelist) )  colnames(whitelist) <- c("from", "to")
    if ( method == "cat" )  {
      for ( i in 1:dim(x)[2] ) x[, i] <- as.factor(x[, i])
    }
    dag <- bnlearn::hc(x, blacklist = mhvale, whitelist = whitelist, score = score, restart = restart)
    score <- bnlearn::score(x = dag, data = x, type = score )
  }
  runtime <- proc.time() - runtime
  colnames(a$G) <- nama      ;       rownames(a$G) <- nama
  list(ini = a, dag = dag, mhvale = mhvale, score = score, runtime = runtime)
}
