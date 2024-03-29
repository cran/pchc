dcor.fedhc.skel <- function(x, alpha = 0.05, ini.stat = NULL, R = NULL) {

  dm <- dim(x)
  n <- dm[1]   ;    d <- dm[2]

  G <- matrix(0, d, d)
  ntests <- 0
  nam <- colnames(x)
  if ( is.null(nam) )  nam <- paste("X", 1:d, sep = "")
  colnames(G) <- nam    ;   rownames(G) <- nam
  la <- log(alpha)

  #suppressWarnings()
  pvalue <- G

  runtime <- proc.time()

  if ( is.null(R)  &  is.null(ini.stat) ) {
    R <- matrix(0, d, d)
    for ( i in 1:(d - 1) ) {
      R[i, -(1:i)] <- as.vector( dcov::mdcor(x[, i], x[, -(1:i), drop = FALSE], type = "U") )
    }
    ntests <- ntests + d * (d - 1) / 2
    R <- R + t(R)
    ini.stat <- n * R + 1
  } else if ( !is.null(R)  &  is.null(ini.stat) ) {
    ini.stat <- n * R + 1
  } else if ( is.null(R)  &  !is.null(ini.stat) ) {
    R <- (ini.stat - 1 ) / n
  }

  ini.pvalue <- pchisq( ini.stat, 1, lower.tail = FALSE, log.p = TRUE)
  diag(ini.pvalue) <- 0

  for (k in 1:d) {
    pval <- ini.pvalue[k, ]
    vars <- which(pval < la)
    if ( length(vars) > 0 ) {
      sela <- which.min(pval)
    } else  sela <- vars
    vars <- setdiff(vars, sela)

    while ( length(vars) > 0 ) {
      for (vim in vars)  {
        stat <- dcov::pdcor( x[, vim], x[, k], x[, sela], type = "U" )
        pval[vim] <- pchisq(n * stat + 1, 1, lower.tail = FALSE, log.p = TRUE)
      }
      ntests <- ntests + length(vars)
      ide <- which(pval[vars] < la)
      vars <- vars[ide]
      sel <- which.min(pval[vars])
      sela <- c(sela, vars[sel] )
      vars <- setdiff(vars, vars[sel])
    } ## end  while ( length(vars) > 0 ) {

    G[k, sela] <- 1
    pvalue[k, ] <- pval
  }  ##  end   for (k in 1:d) {

  a <- which( G == 1  &  t(G) == 1 )
  G[ -a ] <- 0
  pvalue <- pmax( pvalue, t(pvalue) )

  runtime <- proc.time() - runtime

  list(ini.stat = ini.stat, ini.pvalue = ini.pvalue, pvalue = pvalue, runtime = runtime[3], n.tests = ntests, G = G)
}


