big_read <- function(big_path, header = TRUE, sep = ",") {
  x <- bigstatsr::big_read(big_path, header = header, sep = sep, type = "double")
  bigstatsr::big_copy(x)
}
