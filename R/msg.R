#' Standardized error message for invalid property values
#'
#' Generates a detailed error message for invalid values in a property, listing the offending values and the allowed set.
#'
#' @typed invalid: character[1+]
#'   Invalid values found.
#' @typed allowed: character[1+]
#'   Allowed values.
#' @typed property: character[1]
#'   Name of the property being checked (default: 'hesper_opts').
#' @typed full_message: character[1] | NULL
#'  Full message that replace the main message 'Invalid values in {property}' if not NULL (default: NULL).
#' @typedreturn character[3]
#'   Suitable for use with \code{rlang::abort()}.
#'
#' @keywords internal
#'
#' @family msg
msg_invalid_values <- function(
  invalid,
  allowed,
  property = 'hesper_opts',
  full_message = NULL
) {
  #------ Checks

  checkmate::assert_character(invalid, min.len = 1)
  checkmate::assert_character(allowed, min.len = 1)
  checkmate::assert_character(property, len = 1)
  checkmate::assert_character(full_message, len = 1, null.ok = TRUE)

  #------ Prepare error message

  # main message
  if (is.null(full_message)) {
    full_message <- glue::glue(
      'Invalid values in ',
      glue::glue_collapse(property, sep = ', ', last = ', and ')
    )
  }

  err_msg <- c(
    full_message,
    '*' = glue::glue(
      'Following values are not allowed: ',
      glue::glue_collapse(invalid, sep = ', ', last = ', and ')
    ),
    'i' = glue::glue(
      'Values must be one of: ',
      glue::glue_collapse(allowed, sep = ', ', last = ', and ')
    )
  )

  return(err_msg)
}

#' Standardized error message for missing vars in a data frame
#'
#' Generates a detailed error message for when required variables are missing from a data frame.
#'
#' @typed df: character[1]
#'  Name of a data frame.
#' @typed vars: character
#'   Names of missing variables.
#' @typed property: character[1] | NULL
#'   Name of the property being checked (default: NULL).
#' @typed i: logical[1]
#'  Whether to include an informational message about checking available variables (default: TRUE).
#' @typedreturn character[3]
#'   Suitable for use with \code{rlang::abort()}.
#' @keywords internal
#'
#' @family msg
msg_missing_vars <- function(df, vars, property = NULL, i = TRUE) {
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
