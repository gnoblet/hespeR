#' Standardized error message for invalid property values
#'
#' Generates a detailed error message for invalid values in a property, listing the offending values and the allowed set.
#'
#' @param invalid Character vector of invalid values found.
#' @param allowed Character vector of allowed values.
#' @param property Name of the property being checked (default: 'hesper_opts').
#'
#' @return A named character vector suitable for use with \code{rlang::abort()}.
#' @keywords internal
#'
#' @family msg
msg_invalid_values <- function(invalid, allowed, property = 'hesper_opts') {
  #------ Checks

  checkmate::assert_character(invalid, min.len = 1)
  checkmate::assert_character(allowed, min.len = 1)
  checkmate::assert_character(property, min.len = 1)

  #------ Prepare error message
  c(
    glue::glue(
      'Invalid values in ',
      glue::glue_collapse(property, sep = ', ', last = ', and ')
    ),
    '*' = glue::glue(
      'Following values are not allowed: ',
      glue::glue_collapse(invalid, sep = ', ', last = ', and ')
    ),
    'i' = glue::glue(
      'Values must be one of: ',
      glue::glue_collapse(allowed, sep = ', ', last = ', and ')
    )
  )
}

#' Standardized error message for missing vars in a data frame
#'
#' Generates a detailed error message for when required variables are missing from a data frame.
#'
#' @param df A string name of data frame.
#' @param vars A character vector of missing variable names.
#' @param property Name of the property being checked (default: NULL).
#' @return A named character vector suitable for use with \code{rlang::abort()}.
#' @keywords internal
#'
#' @family msg
msg_missing_vars <- function(df, vars, property = NULL) {
  #------ Checks

  checkmate::assert_character(df, min.len = 1)
  checkmate::assert_character(vars, min.len = 1)
  checkmate::assert_character(property, len = 1, null.ok = TRUE)

  #------ Prepare error message
  if (is.null(property)) {
    property <- ''
  } else {
    property <- glue::glue(' from ', property)
  }
  c(
    glue::glue('Missing variables in {df}'),
    '*' = glue::glue(
      'The following variables{property} are missing: ',
      glue::glue_collapse(vars, sep = ', ', last = ', and ')
    ),
    'i' = glue::glue(
      'To check, available variables in the data frame, run `colnames({df})`'
    )
  )
}
