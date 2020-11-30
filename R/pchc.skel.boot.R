pchc.skel.boot <- function(x, method = "pearson", alpha = 0.05, B = 200) {
  G <- pchc::pchc.skel(x = x, method = method, alpha = alpha)$G
  dm <- dim(x)
  n <- dm[1]   ;     p <- dm[2]

  runtime <- proc.time()
  gboot <- matrix(0, nrow = B, ncol = p^2)
  for (i in 1:B) {
    id <- sample(n, n, replace = TRUE)
    gb <- pchc::pchc.skel(x = x[id, ], method = method, alpha = alpha)$G
    gboot[i, ] <- as.vector(gb)
  }  ## end for (i in 1:B)
  runtime <- proc.time() - runtime

  Gboot <- Rfast::colmeans(gboot)
  Gboot <- matrix(Gboot, nrow = p, ncol = p)
  list(G = G, Gboot = Gboot, runtime = runtime)
}
