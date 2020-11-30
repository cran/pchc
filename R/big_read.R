big_read <- function(big_path, header = TRUE, sep = ",") {
  x <- bigmemory::read.big.matrix(big_path, header = header, sep = sep, type = "double")
  bigstatsr::big_copy(x)
}
