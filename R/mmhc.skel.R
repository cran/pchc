mmhc.skel <- function(x, method = "pearson", max_k = 3, alpha = 0.05, robust = FALSE,
                      ini.stat = NULL, R = NULL) {

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

  Rfast2::mmhc.skel(x, method = method, max_k = max_k, alpha = alpha, ini.stat = ini.stat, R = R, parallel = FALSE)
}


