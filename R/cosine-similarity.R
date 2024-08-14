#' Compute cosine similarity matrix
#'
#' @description
#' Similarities are computed among rows or list elements. When \code{x} is a
#' list, it is assumed that each element contains a named vector.
#'
#' @param x A list of named vectors, a matrix, or a sparseMatrix
#' @param y A vector; `\code{y}` will be ignored or rejected if \code{x} is not
#' vector.
#'
#' @export
cosine_similarity <- function(x, ...) {
  UseMethod("cosine_similarity", x)
}



#' @export
cosine_similarity.default <- function(x, y) {
  if (is.null(names(x)) || is.null(names(y))) {
    stopifnot(length(x) == length(y))
    w <- seq_along(x)
  } else {
    w <- intersect(names(x), names(y))
  }
  if (length(w) > 0) {
    return(sum(x[w] * y[w]) / (sqrt(sum(x^2)) * sqrt(sum(y^2))))
  } else {
    return(0)
  }
}


#' @export
cosine_similarity.data.frame <- function(x, value_var, cue_var = "cue", resp_var = "response") {
  lapply(
    split(x, x[[cue_var]]),
    df_to_vec,
    value_var = value_var,
    resp_var = resp_var
  ) |>
    cosine_similarity()
}


#' @export
cosine_similarity.list <- function(x) {
  list2sparseMatrix(x) |>
    cosine_similarity()
}


#' @export
cosine_similarity.matrix <- function(x) {
  normalize_rows(x, norm = "l2") |>
    tcrossprod() |>
    similarity()
}


#' @export
cosine_similarity.dgCMatrix <- function(x) {
  normalize_rows(x, norm = "l2") |>
    tcrossprod() |>
    as.array() |>
    similarity()
}
