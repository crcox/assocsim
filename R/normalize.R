#' Normalize data structures by rows or columns
#'
#' @param x A list of numeric vectors, a matrix, or a sparseMatrix
#' @param norm The norm to apply to each row or column. Either "l1" to make sum
#'   of row = 1, or "l2" to make sum of squared row = 1.
#'
#' @returns Normalized data, structured as the input \code{x} was structured.
#'
#' @name normalize
NULL


#' @rdname normalize
#' @export
normalize_rows <- function(x, norm = c("l1", "l2")) {
  UseMethod("normalize_rows", x)
}


#' @rdname normalize
#' @export
normalize_cols <- function(x, norm = c("l1", "l2")) {
  UseMethod("normalize_cols", x)
}


#' @rdname normalize
#' @export
normalize_rows.list <- function(x, norm = c("l1", "l2")) {
  norm <- match.arg(norm)
  norm_vec <- switch(
    norm,
    l1 = vapply(x, sum, numeric(1)),
    l2 = vapply(x, function(y) sqrt(sum(y^2)), numeric(1))
  )
  norm_vec <- ifelse(norm_vec == 0, 0, 1 / norm_vec)
  return(mapply(function(v, k) v * k, x, norm_vec))
}


#' @export
normalize_rows.matrix <- function(x, norm = c("l1", "l2")) {
  norm <- match.arg(norm)
  norm_vec <- switch(
    norm,
    l1 = rowSums(x),
    l2 = sqrt(rowSums(x^2))
  )
  norm_vec <- ifelse(norm_vec == 0, 0, 1 / norm_vec)
  return(x * norm_vec)
}


#' @export
normalize_rows.dgCMatrix <- function(x, norm = c("l1", "l2")) {
  norm <- match.arg(norm)
  norm_vec <- switch(
    norm,
    l1 = Matrix::rowSums(x),
    l2 = sqrt(Matrix::rowSums(x^2))
  )
  norm_vec <- ifelse(norm_vec == 0, 0, 1 / norm_vec)
  return(Matrix::rowScale(x, norm_vec))
}

#' @export
normalize_cols.list <- function(x, norm = c("l1", "l2")) {
  return(
    list2sparseMatrix(x) |>
      normalize_cols(match.arg(norm)) |>
      sparseMatrix2list()
  )
}


#' @export
normalize_cols.matrix <- function(x, norm = c("l1", "l2")) {
  norm <- match.arg(norm)
  norm_vec <- switch(
    norm,
    l1 = colSums(x),
    l2 = sqrt(colSums(x^2))
  )
  norm_vec <- ifelse(norm_vec == 0, 0, 1 / norm_vec)
  return(t(t(x) * norm_vec))
}


#' @export
normalize_cols.dgCMatrix <- function(x, norm = c("l1", "l2")) {
  norm <- match.arg(norm)
  norm_vec <- switch(
    norm,
    l1 = Matrix::colSums(x),
    l2 = sqrt(Matrix::colSums(x^2))
  )
  norm_vec <- ifelse(norm_vec == 0, 0, 1 / norm_vec)
  return(Matrix::colScale(x, norm_vec))
}
