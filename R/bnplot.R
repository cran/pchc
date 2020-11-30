bnplot <- function(dag, shape = "ellipse", main = NULL, sub = NULL) {
  bnlearn::graphviz.plot(x = dag, shape = shape, main = main, sub = sub)
}
