#' Convert between list and sparse matrix representations
#'
#' @param x A list
#' @param m A sparseMatrix
#'
#' @returns The same data in the complimentary format
#' @details
#' Lists are converted into "C" sparse matrices.
#'
#' @name conversions-list-sparseMatrix
NULL


#' @rdname conversions-list-sparseMatrix
#' @export
list2sparseMatrix <- function(x) {
  colind <- function(r, colnames) {
    which(colnames %in% names(r))
  }
  colnames <- unique(unlist(lapply(x, names)))
  return(Matrix::sparseMatrix(
    i = rep(seq_along(x), lengths(x)),
    j = unlist(lapply(x, colind, colnames = colnames)),
    x = unlist(x, recursive = FALSE, use.names = FALSE),
    dims = c(length(x), length(colnames)),
    dimnames = list(names(x), colnames),
    symmetric = FALSE,
    triangular = FALSE,
    index1 = TRUE,
    repr = "C",
    check = TRUE
  ))
}


#' @rdname conversions-list-sparseMatrix
#' @export
sparseMatrix2list <- function(m) {
  apply(m, 1, function(r) r[r != 0], simplify = FALSE)
}
