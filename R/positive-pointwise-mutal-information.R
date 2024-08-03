positive_pointwise_mutual_information <- function(df) {
  x <- lapply(split(df, df$cue), df_to_vec)
  rsum <- vapply(split(df$R123.Strength, df$response), sum, numeric(1))
  return(lapply(x, ppmi, rsum = rsum, N = length(x)))
}

ppmi <- function(p, rsum, N) {
  setNames(pmax(0, log2((p * N) / rsum[names(p)])), names(p))
}
