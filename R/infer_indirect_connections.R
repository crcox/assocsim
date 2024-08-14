
#' @export
infer_indirect_connections <- function(x, alpha = 0.75) {
  UseMethod("infer_indirect_connections", x)
}


#' @export
infer_indirect_connections.dgCMatrix <- function(x, alpha = 0.75) {
  x <- ppmi(x)
  x <- normalize_rows(x, norm = "l1")
  # katzWalk is very slow!
  x <- katzWalk(x, alpha)
  x <- ppmi(x)
  x <- normalize_rows(x, norm = "l1")
  return(x)
}


#' @export
infer_indirect_connections.matrix <- infer_indirect_connections.dgCMatrix


# Add indirect paths using Katz walks
# Note: this function is very slow in R! Use matlab script instead
#' @export
katzWalk <- function(x, alpha){
  stopifnot(is.matrix(x) || inherits(x, "sparseMatrix"))
  I <- diag(nrow(x))
  k <- solve(I - (alpha * x))
  k@Dimnames <- x@Dimnames
  return(k)
}
