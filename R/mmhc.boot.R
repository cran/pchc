mmhc.boot <- function(x, method = "pearson", max_k = 3, alpha = 0.05, ini.stat = NULL,
                      R = NULL, restart = 10, score = "bic-g", blacklist = NULL, whitelist = NULL, B = 200) {

  mod <- pchc::mmhc(x, method = method, max_k = max_k, alpha = alpha, ini.stat = ini.stat, R = R, restart = restart,
                    score = score, blacklist = blacklist, whitelist = whitelist)
  dm <- dim(x)
  n <- dm[1]   ;     p <- dm[2]

  runtime <- proc.time()
  Gboot <- matrix(0, p, p)
  for (i in 1:B) {
    id <- Rfast2::Sample.int(n, n, replace = TRUE)
    gb <- pchc::mmhc(x[id, ], method = method, max_k = max_k, alpha = alpha, restart = restart,
                     score = score, blacklist = blacklist, whitelist = whitelist)
    Gboot <- Gboot + pchc::bnmat(gb$dag)
  }  ## end for (i in 1:B)
  runtime <- proc.time() - runtime

  list(mod = mod, Gboot = Gboot/B, runtime = runtime)
}
