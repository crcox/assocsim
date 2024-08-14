#' @export
similarity_matrix <- function(df, to_ppmi = FALSE) {
  if (to_ppmi) {
    x <- positive_pointwise_mutual_information(df)
  } else {
    x <- lapply(split(df, df$cue), df_to_vec)
  }
  z <- lower.tri(matrix(nrow = length(x), length(x)))
  cosine_similarity(x)[z]
}
