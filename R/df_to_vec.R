df_to_vec <- function(df) {
  setNames(df[["R123.Strength"]], df[["response"]])
}
