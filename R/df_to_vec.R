df_to_vec <- function(df, value_var, resp_var) {
  setNames(df[[value_var]], df[[resp_var]])
}
