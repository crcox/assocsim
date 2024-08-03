associative_strength <- function(x, rescale = NULL) {
  if (is.data.frame(x)) {
    df <- x
    x <- lapply(split(df, x$cue), df_to_vec)
    if (!is.null(rescale) && rescale == "ppmi") {
      rsum <- vapply(split(df$R123.Strength, df$response), sum, numeric(1))
      x <- lapply(x, ppmi, N = length(x), rsum = rsum)
    }
  }
  cues <- names(x)
  ix_pairs <- combn(length(cues), 2)
  apply(ix_pairs, 2, astrength, x = x)
}

astrength <- function(ij, x) {
  a <- x[[ij[1]]]
  b <- x[[ij[2]]]
  w <- intersect(names(a), names(b))
  if (length(w) > 0) {
    return(sum(a[w] * b[w]) / (sqrt(sum(a[w]^2)) * sqrt(sum(b[w]^2))))
  } else {
    return(0)
  }
}
