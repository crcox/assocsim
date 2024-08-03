associative_strength <- function(df, to_ppmi = FALSE) {
  if (to_ppmi) {
    x <- positive_pointwise_mutual_information(df)
  } else {
    x <- lapply(split(df, df$cue), df_to_vec)
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
