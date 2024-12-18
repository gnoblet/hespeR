recode_subset_to_missing <- function(df, vars, subset_var, subset_vals, suffix = NULL, missing_code=NULL) {

  #------ Checks

  # df is a dataframe
  checkmate::assertDataFrame(df)

  # df is not data.table, convert it
  if (!checkmate::testDataTable(df)) {
    rlang::warn("Converting df to data.table.")
    data.table::setDT(df)
  }

  # vars is character and not missing
  checkmate::assertCharacter(vars, min.chars = 1, any.missing = FALSE)

  # subset_var is character of length 1
  checkmate::assertCharacter(subset_var, len = 1, min.chars = 1, any.missing = FALSE)

  # subset_vals is a character vector
  checkmate::assertCharacter(subset_vals, min.chars = 1, any.missing = FALSE)

  # vars are in df
  check_vars_in_df(df, vars)

  # subset_var is in df
  check_vars_in_df(df, subset_var)

  # warn if none of the subset_var values are in subset_vals
  warn_var_no_vals(df, subset_var, subset_vals)

  # suffix is a character scalar or NULL
  checkmate::assertCharacter(suffix, len = 1, null.ok = TRUE)


  #------ Recode

  # prepare new_vars
  if (is.null(suffix)) {
    new_vars <- vars
  } else {
    new_vars <- paste0(vars, suffix)
  }

  # if missing_code not null with character value, use it instead of NA_character_
  if (!is.null(missing_code)) {
    checkmate::assertCharacter(missing_code, len = 1, min.chars = 1, any.missing = FALSE)
  } else {
    missing_code <- NA_character_
  }

  # recode
  df[,
    (new_vars) := lapply(.SD, \(x) ifelse(!get(subset_var) %in% subset_vals, missing_code, x)),
    .SDcols = vars]

  return(df)
}
