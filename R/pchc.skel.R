pchc.skel <- function (x, method = "pearson", alpha = 0.05,
          ini.stat = NULL, ini.pvalue = NULL) {

  Rfast::pc.skel(x, method, alpha, R = 1, ini.stat, ini.pvalue)
}
