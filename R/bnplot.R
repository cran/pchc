bnplot <- function(dag, shape = "circle", main = NULL, sub = NULL) {
  bnlearn::graphviz.plot(x = dag, shape = shape, main = main, sub = sub)
}
