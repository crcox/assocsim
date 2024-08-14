#' Positive pointwise mutual information for responses conditioned on cues
#'
#' @param x A data.frame, list, matrix, or sparseMatrix containing association
#'   strengths.
#' @param value_var,cue_var,resp_var If \code{x} is a data.frame, the names
#'   of these three critical columns need to be provided.
#' @param rprob The mean response strength over cues (i.e., average probability
#'   of each response, where each response has been conditioned on all cues).
#'
#' @details
#' If supplying \code{rprob}, it should be computed as follows:
#' 1. Let X be a matrix, with rows corresponding to cues and columns to responses.
#' 2. Normalize each row of X to sum to 1.
#' 3. Compute column means of X. This vector is \code{rprob}.
#'
#' You may wish to compute \code{rprob} based on a larger, normative dataset to
#' more accurately account for the overall probability of each response over a
#' large set of cues.
#'
#' @export
ppmi <- function(x, ...) {
  UseMethod("ppmi", x)
}

#' @export
ppmi.data.frame <- function(x, value_var, cue_var = "cue", resp_var = "response", as_list = FALSE, rprob = NULL) {
  p <- lapply(
    split(x, x[[cue_var]]),
    df_to_vec,
    value_var = value_var,
    resp_var = resp_var
  ) |>
    ppmi(rprob)

  if (as_list) {
    return(p)
  } else {
    df <- data.frame(
      cue = names(p)[lengths(p)] %||% rep(seq_along(p), each = lengths(p)),
      respones = unlist(lapply(p, names))
    )
    names(df) <- c(cue_var, resp_var)
    df <- merge(df, x, by = c(cue_var, resp_var))
    df$ppmi <- unlist(p)
    return(df)
  }
}

#' @export
ppmi.numeric <- function(x, rprob) {
  if (is.null(names(x)) || is.null(names(rprob))) {
    stopifnot(length(x) == length(rprob))
    w <- seq_along(x)
  } else {
    w <- intersect(names(x), names(rprob))
  }
  x <- matrix(x[w], nrow = 1, dimnames = list(NULL, w))
  return(ppmi(x, rprob[w])[1,])
}


#' @export
ppmi.list <- function(x, rprob = NULL) {
  list2sparseMatrix(x) |>
    ppmi(rprob) |>
    sparseMatrix2list()
}


#' @export
ppmi.dgCMatrix <- function(x, rprob = NULL) {
  if (is.null(rprob)) {
    rprob_inv <- nrow(x) / Matrix::colSums(x)
  } else {
    rprob_inv <- 1 / rprob
  }
  x <- Matrix::rowScale(x, 1 / Matrix::rowSums(x))
  x <- Matrix::colScale(x, rprob_inv)
  x@x <- log2(x@x)
  return(pmax(x, 0))
}


#' @export
ppmi.matrix <- function(x, rprob = NULL) {
  x <- x / rowSums(x)
  x <- t(t(x) / (rprob %||% colMeans(x)))
  x <- log2(x)
  return(pmax(x, 0))
}
