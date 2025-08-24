#' Rename HESPER item columns to standardized names
#'
#' This function renames HESPER item columns in a data frame to a standardized set of allowed names.
#'
#' @typed df: data.frame[,1+]
#'   A data frame containing HESPER items.
#' @typed old_hesper_vars: character_named[1+]
#'   Column names mapped to allowed HESPER item names. Names are new (standardized) names, values are old names to rename from.
#'
#' @typedreturn data.frame[,1+]
#'   A data frame with HESPER item columns renamed to the allowed names.
#'
#' @export
rec_hesper_vars <- function(df, old_hesper_vars) {
  #------ Checks
  checkmate::assert_data_frame(df)
  checkmate::assert_character(
    old_hesper_vars,
    any.missing = FALSE,
    names = "named"
  )
  checkmate::assert_subset(old_hesper_vars, colnames(df))
  allowed_vars <- hesper_vars()
  check_values_in_set(
    names(old_hesper_vars),
    allowed_vars,
    property = "old_hesper_vars"
  )

  #------ Rename columns

  new_names <- purrr::set_names(names(old_hesper_vars), old_hesper_vars)
  which_names <- colnames(df) %in% old_hesper_vars
  colnames(df)[which_names] <- new_names[colnames(df)[which_names]]

  return(df)
}
