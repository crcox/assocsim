#' Class for representing symmetric similarity matrix as lower-triangle vector
#'
#' @description
#' This is a simple adaptation of the \code{dist} S3 class
#'
#' @param x A symmetric similarity matrix
#' @export
similarity <- function(x) {
  stopifnot(isSymmetric(x))
  obj <- x[lower.tri(x)]
  attributes(obj) <- list(
    Size = nrow(x),
    Labels = rownames(x),
    Diag = FALSE,
    Upper = FALSE,
    call = match.call(),
    method = "cosine"
  )
  class(obj) <- "similarity"
  return(obj)
}


#' @export
as.data.frame.similarity <- function(x) {
  ij <- combn(attr(x, "Size"), 2)
  if (attr(x, "Upper")) {
    i <- ij[2, ]
    j <- ij[1, ]
  } else {
    i <- ij[1, ]
    j <- ij[2, ]
  }
  df <- data.frame(
    x = attr(x, "Labels")[i] %||% i,
    y = attr(x, "Labels")[j] %||% j,
    sim = unclass(x)
  )
  return(df)
}


#' @export
as.matrix.similarity <- function(x) {
  size <- attr(x, "Size")
  S <- diag(size)
  N <- size^2
  if (size > 1L) {
    n..1 <- (size - 1L):1L
    s.1 <- size + 1L
    up <- sequence.default(n..1, from = seq.int(s.1, N, s.1), by = size)
    lo <- sequence.default(n..1, from = seq.int(2L, N + 1L, s.1))
    S[up] <- S[lo] <- x
  }
  labels <- attr(x, "Labels") %||% seq_len(size)
  dimnames(S) <- list(labels, labels)
  return(S)
}


#' @export
print.similarity <- function(x, digits = getOption("digits"), justify = "none", quote = FALSE, right = TRUE) {
  if (length(x)) {
    diag <- attr(x, "Diag") %||% FALSE
    upper <- attr(x, "Upper") %||% FALSE
    m <- as.matrix(x)
    cf <- format(m, digits = digits, justify = justify)
    if (!upper)
      cf[row(cf) < col(cf)] <- ""
    if (!diag)
      cf[row(cf) == col(cf)] <- ""

    msg <- if (diag || upper) cf
    else cf[-1, -attr(x, "Size"), drop = FALSE]
    print(msg, quote = quote, right = right)
    cat("similarity method: ", attr(x, "method"), "\n")
  } else {
    cat(data.class(x), "(0)\n", sep = "")
  }
  invisible(x)
}
