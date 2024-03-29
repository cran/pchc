mmtabu <- function(x, method = "pearson",  max_k = 3, alpha = 0.05, robust = FALSE, skel = NULL, ini.stat = NULL,
                 R = NULL, tabu = 10, score = "bic-g", blacklist = NULL, whitelist = NULL) {

  runtime <- proc.time()
  if ( robust ) {
    dm <- dim(x)
    n <- dm[1]    ;   p <- dm[2]
    mod <- robustbase::covMcd( x, alpha = ceiling( 0.5 * (n + p + 1) )/n )
    w <- sum( mod$mcd.wt )
    d1 <- w / (w - 1)^2 * mod$mah[mod$mcd.wt == 1]
    d0 <- w / (w + 1) * (w - p) / ( (w - 1) * p ) * mod$mah[mod$mcd.wt == 0]
    ep1 <- which( d1 > qbeta(0.975, 0.5 * p, 0.5 * (w - p - 1) ) )
    ep0 <- which( d0 > qf(0.975, p, w - p) )
    poia <- c( which(mod$mcd.wt == 1)[ep1],  which(mod$mcd.wt == 0)[ep0] )
    x <- x[-poia, ]
    n <- dim(x)
    R <- cor(x)
  }
  if ( method == "cat"  &  !is.matrix(x) )  {
    for ( i in 1:dim(x)[2] ) x[, i] <- as.numeric(x[, i]) - 1
    x <- Rfast::data.frame.to_matrix(x, col.names = colnames(x) )
  }
  if ( method == "pearson"  &  is.null(ini.stat)  &  !is.null(R) ) {
    ini.stat <- 0.5 * log( (1 + R)/( (1 - R) ) ) * sqrt(n - 3)
  }
  ## score for continuous : "bic-g" (default), "loglik-g", "aic-g", "bge"
  ## score for discrete : "bic", "loglik", "bde"
  if ( is.null(skel) ) {
    a <- pchc::mmhc.skel(x, method = method, max_k = max_k, alpha = alpha, ini.stat = ini.stat, R = R)
  } else  a <- skel
  nama <- colnames(x)
  if ( is.null(nama) )  nama <-  paste("X", 1:dim(x)[2], sep = "")
  colnames(x) <- nama
  vale <- which(a$G == 1)
  dag <- NULL
  scoring <- NULL
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
      for ( i in 1:dim(x)[2] )  x[, i] <- as.factor(x[, i])
    }
    dag <- bnlearn::tabu(x, blacklist = mhvale, whitelist = whitelist, score = score, tabu = tabu)
    scoring <- bnlearn::score(x = dag, data = x, type = score )
  }
  runtime <- proc.time() - runtime
  colnames(a$G) <- nama      ;       rownames(a$G) <- nama
  list(ini = a, dag = dag, scoring = scoring, runtime = runtime[3])
}
