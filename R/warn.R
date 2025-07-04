#' Warn and replace existing variables in a data frame
#'
#' @param df The data frame to check for existing variables
#' @param vars A character vector of variable names to check for
#'
#' @details This function checks if any of the variables in `vars` are already present in `df`.
#' If any are found, a warning is issued and the variables are replaced from `df`.
#'
#' @examples
#' df <- data.frame(x = 1, y = 2)
#' warn_replace(df, c("x", "z"))
warn_replace <- function(df, vars) {
  warn_var_msg(df, vars, "Variable(s) exist in df and will be replaced.")
}


#' Warn and remove existing variables from a data frame
#'
#' @param df The data frame to check for existing variables
#' @param vars A character vector of variable names to check for
#'
#' @details This function checks if any of the variables in `vars` are already present in `df`.
#' If any are found, a warning is issued and the variables will be removed from `df`.
#'
#' @examples
#' df <- data.frame(x = 1, y = 2)
#' warn_removal(df, c("x", "z"))
warn_removal <- function(df, vars) {
  warn_var_msg(df, vars, "Variable(s) exist in df and will be removed.")
}


#' Warn message
#'
#' @param df The data frame to check for existing variables
#' @param vars A character vector of variable names to check for
#' @param msg The message to print
#'
#' @details This function checks if any of the variables in `vars` are already present in `df`.
#' If any are found, a warning is issued and the variables will be removed from `df`.
#'
#' @examples
#' df <- data.frame(x = 1, y = 2)
#' warn_removal(df, c("x", "z"), "Variable(s) will be removed.")
warn_var_msg <- function(df, vars, msg) {
  vars_in_lgl <- vars %in% colnames(df)
  if (any(vars_in_lgl)) {
    vars_in <- vars[vars_in_lgl]
    rlang::warn(c(
      msg,
      "*" = glue::glue(
        "Variable(s): ",
        glue::glue_collapse(vars_in, sep = ", ", last = ", and "),
        "."
      )
    ))
  }
}


#' Warn if a variable contains no values in a set
#'
#' @param df The data frame to check for existing variables
#' @param var A variable name
#' @param set A character vector of values to check for
#'
#' @details This function checks if any of the variables in `vars` contain no values in `set`.
#'
#' @export
warn_var_no_vals <- function(df, var, vals) {
  vals_nin <- setdiff(df[[var]], vals)
  if (length(vals_nin) == length(df[[var]])) {
    rlang::warn(
      glue::glue(
        "Variable {var} contains no values in the set of vals:",
        glue::glue_collapse(vals, sep = ", ", last = ", and ")
      )
    )
  }
}
