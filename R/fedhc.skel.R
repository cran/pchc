fedhc.skel <- function(x, method = "pearson", alpha = 0.05, robust = FALSE,
                     ini.stat = NULL, R = NULL, parallel = FALSE) {

  dm <- dim(x)
  n <- dm[1]   ;    d <- dm[2]

  if ( robust  &  method == "pearson" ) {
    mod <- robustbase::covMcd( x, alpha = ceiling( 0.5 * (n + d + 1) )/n )
    w <- sum( mod$mcd.wt )
    d1 <- w / (w - 1)^2 * mod$mah[mod$mcd.wt == 1]
    d0 <- w / (w + 1) * (w - d) / ( (w - 1) * d ) * mod$mah[mod$mcd.wt == 0]
    ep1 <- which( d1 > qbeta(0.975, 0.5 * d, 0.5 * (w - d - 1) ) )
    ep0 <- which( d0 > qf(0.975, d, w - d) )
    poia <- c( which(mod$mcd.wt == 1)[ep1],  which(mod$mcd.wt == 0)[ep0] )
    x <- x[-poia, ]
	  n <- dim(x)[1]
  }

  Rfast2::fedhc.skel(x, method = method, alpha = alpha, ini.stat = ini.stat, R = R, parallel = FALSE)
}




# fedhc.skel <- function(x, method = "pearson", alpha = 0.05, robust = FALSE,
#                      ini.stat = NULL, R = NULL) {
#
#   dm <- dim(x)
#   n <- dm[1]   ;    d <- dm[2]
#
#   if ( robust  &  method == "pearson" ) {
#     mod <- robustbase::covMcd( x, alpha = ceiling( 0.5 * (n + d + 1) )/n )
#     w <- sum( mod$mcd.wt )
#     d1 <- w / (w - 1)^2 * mod$mah[mod$mcd.wt == 1]
#     d0 <- w / (w + 1) * (w - d) / ( (w - 1) * d ) * mod$mah[mod$mcd.wt == 0]
#     ep1 <- which( d1 > qbeta(0.975, 0.5 * d, 0.5 * (w - d - 1) ) )
#     ep0 <- which( d0 > qf(0.975, d, w - d) )
#     poia <- c( which(mod$mcd.wt == 1)[ep1],  which(mod$mcd.wt == 0)[ep0] )
#     x <- x[-poia, ]
# 	  n <- dim(x)[1]
#   }
#
#   G <- matrix(0, d, d)
#   ntests <- 0
#   nam <- colnames(x)
#   if ( is.null(nam) )  nam <- paste("X", 1:d, sep = "")
#   colnames(G) <- nam    ;   rownames(G) <- nam
#   la <- log(alpha)
#
#   oop <- options(warn = -1)
#   on.exit( options(oop) )
#   pvalue <- G
#
#   runtime <- proc.time()
#
#   if ( method == "cat"  &  !is.matrix(x) )  {
#     for ( i in 1:dim(x)[2] ) x[, i] <- as.numeric(x[, i]) - 1
#     x <- as.matrix(x)
#   }
#
#   if ( method == "pearson" ) {
#     if ( is.null(ini.stat)  &  is.null(R) ) {
#       R <- cor(x)
#       ini.stat <- 0.5 * log( (1 + R)/( (1 - R) ) ) * sqrt(n - 3)
#     } else {
#       if ( !is.null(ini.stat)  &  is.null(R) ) {
#         R <- (ini.stat - 1) / (ini.stat + 1)
#       } else if ( is.null(ini.stat)  &  !is.null(R) ) {
#         ini.stat <- 0.5 * log( (1 + R)/( (1 - R) ) ) * sqrt(n - 3)
#       }
#     }  ##  end  if ( is.null(ini.stat)  &  is.null(R) )
#     ini.pvalue <- log(2) + pt( abs(ini.stat), n - 3, lower.tail = FALSE, log.p = TRUE)
#     diag(ini.pvalue) <- 0
#     ntests <- ntests + d * (d - 1) / 2
#
#   } else  if ( method == "cat" ) {
#     dc <- Rfast::colrange(x, cont = FALSE)
#     mod <- Rfast::g2Test_univariate(x, dc)
#     ini.stat <- mod$statistic
#     ini.pvalue <- pchisq(ini.stat, mod$df, lower.tail = FALSE, log.p = TRUE)
#     ini.pvalue <- Rfast::squareform(ini.pvalue)
#     ntests <- ntests + d * (d - 1) / 2
#   }  ##  end  if ( method == "pearson" ) {
#
#   if ( method == "pearson" ) {
#
#   for (k in 1:d) {
#     pval <- ini.pvalue[k, ]
#     vars <- which(pval < la)
#     if ( length(vars) > 0 ) {
#       sela <- which.min(pval)
#     } else  sela <- vars
#     vars <- setdiff(vars, sela)
#
#     while ( length(vars) > 0 ) {
#       for (vim in vars)   pval[vim] <- pchc::pcor(R, indx = vim, indy = k, indz = sela, n)[2]
#       ntests <- ntests + length(vars)
#       ide <- which(pval[vars] < la)
#       vars <- vars[ide]
#       sel <- which.min(pval[vars])
#       sela <- c(sela, vars[sel] )
#       vars <- setdiff(vars, vars[sel])
#     } ## end  while ( length(vars) > 0 ) {
#
#     G[k, sela] <- 1
#     pvalue[k, ] <- pval
#   }  ##  end   for (k in 1:d) {
#
#   } else if ( method == "cat" ) {
#
#     for (k in 1:d) {
#       pval <- ini.pvalue[k, ]
#       vars <- which(pval < la)
#       if ( length(vars) > 0 ) {
#         sela <- which.min(pval)
#       } else  sela <- vars
#       vars <- setdiff(vars, sela)
#
#       while ( length(vars) > 0 ) {
#         for (vim in vars) {
#           sp <- Rfast::g2Test(x, vim, k, sela, dc)
#           pval[vim] <- pchisq(sp$statistic, sp$df, lower.tail = FALSE, log.p = TRUE)
#         }
#         ntests <- ntests + length(vars)
#         ide <- which(pval[vars] < la)
#         vars <- vars[ide]
#         sel <- which.min(pval[vars])
#         sela <- c(sela, vars[sel] )
#         vars <- setdiff(vars, vars[sel])
#       } ## end  while ( length(vars) > 0 ) {
#       G[k, sela] <- 1
#       pvalue[k, ] <- pval
#     }  ##  end   for (k in 1:d) {
#
#   }  ##  end  if ( method == "pearson" ) {
#
#   a <- which( G == 1  &  t(G) == 1 )
#   G[ -a ] <- 0
#   pvalue <- pmax( pvalue, t(pvalue) )
#
#   runtime <- proc.time() - runtime
#
#   list(ini.stat = ini.stat, ini.pvalue = ini.pvalue, pvalue = pvalue, runtime = runtime[3], n.tests = ntests, G = G)
# }
#
#
