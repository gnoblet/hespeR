#' Sum variables in a set of values
#'
#' This function sums the number of times a set of values (vals) appears across a set of variables (vars) in a data frame into a new variable (new_var).
#'
#' @param df A data frame to modify.
#' @param vars A character vector of variables.
#' @param vals A character vector of values.
#' @param suffix A character scalar or an empty string to append to the variable names. Defaults to NULL.
#'
#' @return The modified data frame with undefined values replaced by NA. If suffix is NULL or an empty string, variables vars are modified; otherwise, new variables are added with suffix.
#'
#' @export
sum_vals_across <- function(df, vars, vals, new_var) {
  #------ Checks

  # df is a dataframe
  checkmate::assertDataFrame(df)

  # df is not data.table, convert it
  if (!checkmate::testDataTable(df)) {
    rlang::warn("Converting df to data.table.")
    data.table::setDT(df)
  }

  # vars is a character vector
  checkmate::assertCharacter(vars, min.chars = 1, any.missing = FALSE)

  # vals a character vector
  checkmate::assertCharacter(vals, min.chars = 1, any.missing = FALSE)

  # all vars are in df and of class character
  check_vars_in_df(df, vars)

  # new_var is a character scalar or an empty string
  checkmate::assertCharacter(new_var, len = 1, any.missing = FALSE)

  # warn for replacement if new_var exist
  if (new_var %in% colnames(df)) {
    rlang::warn(glue::glue(
      "Variable {new_var} already exist in df. It will be replaced."
    ))
  }

  #------ Add new_var count
  df[,
    (new_var) := sum(.SD %in% vals, na.rm = TRUE),
    .SDcols = vars,
    by = .I
  ]

  return(df)
}
