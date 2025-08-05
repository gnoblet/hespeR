#' Check if values in a vector are all in a given set of allowed values
#'
#' @typed x: vector[1+]
#'   Values to check.
#' @typed allowed: vector[1+]
#'   Allowed values.
#' @typed property: character[1]
#'   Name of the property being checked (default: 'hesper_opts').
#'
#' @typedreturn TRUE | error
#'   TRUE if all values are in the allowed set, otherwise throws an error.
#'
#' @keywords internal
check_values_in_set <- function(x, allowed, property = 'hesper_opts') {
  #------ Checks

  checkmate::assert_vector(x, min.len = 1)
  checkmate::assert_vector(allowed, min.len = 1)

  #------ Abort or not
  if (!all(x %in% allowed)) {
    invalid <- setdiff(x, allowed)
    rlang::abort(
      msg_invalid_values(invalid, allowed, property = property)
    )
  }
  return(TRUE)
}

#' Check for missing variables in a data frame
#'
#' @typed df: data.frame[,1+]
#'   A data frame
#' @typed vars: character[1+]
#'   Names of variables to check for in the data frame.
#' @typed property: character[1] | NULL
#'  Property being checked (default: 'hesper_vars').
#'
#' @typedreturn TRUE | error
#'   TRUE if all variables are present, otherwise throws an error.
#'
#' @keywords internal
check_missing_vars <- function(df, vars, property = 'hesper_vars') {
  #------ Checks

  # df is a data frame
  checkmate::assert_data_frame(df, min.cols = 1)

  # vars is a character vector
  checkmate::assert_character(vars, min.len = 1)

  # property is a character scalar or NULL
  checkmate::assert_character(property, len = 1, null.ok = TRUE)

  #------ Check for missing vars

  vars_nin <- setdiff(vars, colnames(df))
  if (length(vars_nin) > 0) {
    rlang::abort(
      msg_missing_vars('df', vars_nin, property = property)
    )
  }

  return(TRUE)
}

#' Check if items of vectors in a list are a set of allowed values
#'
#' This function checks if the values in specified variables of a data frame are all within a given set of allowed values. If any values are not in the set, it generates an error message detailing the invalid values in which vars and the expected set.
#'
#' @typed l_x: list_named_vector[1+]
#'   A list of named vectors to check.
#' @typed vars: character[1+]
#'  Names of variables to check in the data frame.
#' @typed set: vector[1+]
#'   Allowed values that the variables should contain.
#'
#' @typedreturn TRUE | error
#'   TRUE if all values in the specified variables are in the set, otherwise throws an error.
#'
#' @keywords internal
check_vecs_in_set <- function(
  l_x,
  set
) {
  #------ Checks

  # l_x is a named list of vectors
  checkmate::assert_list(l_x, min.len = 1, names = "named")

  #------ Values not in set
  values_lgl <- purrr::map_lgl(
    l_x,
    \(x) {
      !all(stats::na.omit(unique(x)) %in% set)
    }
  )

  if (any(values_lgl)) {
    l_x <- l_x[values_lgl]
    # Get values not in set for each item in l_x
    values_chr <- purrr::map_chr(l_x, function(x) {
      x <- unique(x)
      x[!is.na(x) & !(x %in% set)]
    })

    # Error message
    rlang::abort(msg_invalid_values(
      values_chr,
      set,
      property = names(l_x)
    ))
  }

  return(TRUE)
}

#' Check if variables in a data frame are in a set of allowed values
#'
#' This function checks if the values in specified variables of a data frame are all within a given set of allowed values. If any values are not in the set, it generates an error message detailing the invalid values in which vars and the expected set.
#'
#' @typed df: data.frame[,1+]
#'   A data frame
#' @typed vars: character[1+]
#'  Names of variables to check in the data frame.
#' @typed set: vector[1+]
#'   Allowed values that the variables should contain.
#'
#' @typedreturn TRUE | error
#'   TRUE if all values in the specified variables are in the set, otherwise throws an error.
#'
#' @keywords internal
check_vars_in_set <- function(
  df,
  vars,
  set
) {
  #------ Checks

  # df is a data frame
  checkmate::assert_data_frame(df)

  # vars is a character vector
  checkmate::assert_character(vars, min.len = 1)

  # missing vars
  check_missing_vars(df, vars)

  #------ Values not in set
  values_lgl <- purrr::map_lgl(
    df[vars],
    \(x) {
      !all(stats::na.omit(unique(x)) %in% set)
    }
  )

  if (any(values_lgl)) {
    vars <- vars[values_lgl]
    # Get values not in set for each column in df[vars]
    values_chr <- purrr::map_chr(df[vars], function(x) {
      x <- unique(x)
      x[!is.na(x) & !(x %in% set)]
    })

    # Error message
    rlang::abort(msg_invalid_values(values_chr, set, property = vars))
  }

  return(TRUE)
}

#' Check duplicate values in a vector
#'
#' This function checks for duplicate values in a vector and throws an error if any duplicates are found.
#'
#' @typed vec: vector
#'  The vector to check for duplicates.
#' @typed property: character[1]
#'  The name of the property being checked (default: 'vector').
#' @typedreturn TRUE | error
#'  TRUE if no duplicates are found, otherwise throws an error with a message listing the duplicate values.
#' @keywords internal
check_dupes <- function(vec, property = 'vector') {
  #------ Checks

  # vec is a vector
  checkmate::assert_vector(vec)

  # property is a character scalar
  checkmate::assert_character(property, len = 1)

  #------ Check for duplicates
  dupes_vec <- unique(vec[duplicated(vec)])
  if (length(dupes_vec) > 0) {
    rlang::abort(c(
      "Duplicate values found in {property}.",
      "*" = paste0(
        "Duplicate values: ",
        glue::glue_collapse(dupes_vec, sep = ", ", last = ", and ")
      )
    ))
  }

  return(TRUE)
}

#' Check class of a list of items
#'
#' This function checks if all elements in a list are of a specified class. If any element is not of the specified class, it returns an error message.
#'
#' @typed self: list[1+]
#'  A list of items to check.
#' @typed class_name: character[1]
#'  The name of the class to check against.
#' @typedreturn TRUE | error
#'  TRUE if all elements are of the specified class, otherwise throws an error.
#' @keywords internal
#'
check_list_class <- function(self, class_name) {
  #------ Checks
  # self is a list of at least one element
  checkmate::assert_list(self, min.len = 1)
  # class_name is a character scalar
  checkmate::assert_character(class_name, len = 1)
  #------ Check class of each element in the list
  lgl_class <- purrr:map_lgl(self, ~ S7::class(.) == class_name)
  if (!all(lgl_class)) {
    non_class_items <- self[!lgl_class]

    rlang::abort(c(
      "Not all elements in the list are of the specified class.",
      "*" = paste0(
        "Non-",
        class_name,
        " items: ",
        paste(non_class_items, collapse = ", ")
      )
    ))
  }

  return(TRUE)
}

# #' @title Check if variables are in data frame
# #'
# #' @param df A data frame
# #' @param vars A vector of variable names
# #'
# #' @return A stop statement
# check_vars_in_df <- function(df, vars, warn = F) {
#   vars_nin <- setdiff(vars, colnames(df))
#   if (length(vars_nin) > 0) {
#     rlang::abort(glue::glue(
#       "Variables ",
#       glue::glue_collapse(vars_nin, sep = ", ", last = ", and "),
#       " not found in data frame."
#     ))
#   }
# }

# #' @title Check for duplicate values
# #'
# #' @param vec A vector
# #' @param msg A message
# #'
# #' @return A stop statement
# check_dupes <- function(vec, msg) {
#   dupes_vec <- unique(vec[duplicated(vec)])
#   if (length(dupes_vec) > 0) {
#     rlang::abort(
#       glue::glue(
#         msg,
#         glue::glue_collapse(dupes_vec, sep = ", ", last = ", and "),
#       )
#     )
#   }
# }

# #' @title Check if variables are of any given class(es)
# #'
# #' @param df A data frame
# #' @param vars A vector of variable names
# #' @param class A vector of classes
# #'
# #' @return A stop statement
# check_vars_class_in_df <- function(df, vars, class) {
#   vars_not_class <- vars[
#     !sapply(df[, vars], function(x) checkmate::testClass(x, class))
#   ]
#   if (length(vars_not_class) > 0) {
#     rlang::abort(glue::glue(
#       "Variables ",
#       glue::glue_collapse(vars_not_class, sep = ", ", last = ", and "),
#       " are not of class ",
#       glue::glue_collapse(class, sep = ", ", last = ", or "),
#       "."
#     ))
#   }
# }

# #' @title Check if lists of subset are in the right format
# #'
# #' @param sv_l A list of subset dictionaries
# #'
# #' @return A stop statement
# check_sv_l <- function(sv_l, df, hesper_vars, warn_subset_val_no_match = F) {
#   sv_l_assess <- list()

#   for (sv_el in names(sv_l)) {
#     # Get sublist
#     sublist <- sv_l[[sv_el]]

#     # sublist are lists
#     if (!is.list(sublist)) {
#       return(paste(sv_el, "is not a list"))
#     }
#     # sublist is of length 3
#     if (length(sublist) != 3) {
#       rlang::abort(c(
#         paste("Subset dictionary", sv_el, "does not have exactly 3 items."),
#         "*" = paste(
#           "Subset dictionary",
#           sv_el,
#           "has",
#           length(sublist),
#           "item(s)."
#         ),
#         "i" = "It should consist of hesper_vars (vector of hesper variables), subset_var (variable to use for subset) and subset_vals (vector of subset values corresponding to subset_var)."
#       ))
#     }

#     # sublist items are named correctly
#     sublist_names <- names(sublist)[
#       !(names(sublist) %in% c("hesper_vars", "subset_var", "subset_vals"))
#     ]
#     if (length(sublist_names) > 0) {
#       rlang::abort(c(
#         paste(
#           "Subset dictionary",
#           sv_el,
#           "does not have correctly named items"
#         ),
#         "*" = paste(
#           "Subset dictionary",
#           sv_el,
#           "has the following wrongly named item(s):",
#           paste0(sublist_names, collapse = ", "),
#           "."
#         ),
#         "i" = "It should consist of hesper_vars (vector of hesper variables), subset_var (variable to use for subset) and subset_vals (vector of subset values corresponding to subset_var)."
#       ))
#     }

#     # sublist element "hesper_vars is a character vector and one of hesper_vars
#     checkmate::assertCharacter(sublist[["hesper_vars"]], min.chars = 1)
#     hesper_vars_names <- sublist[["hesper_vars"]][
#       !(sublist[["hesper_vars"]] %in% hesper_vars)
#     ]
#     if (length(hesper_vars_names) > 0) {
#       rlang::abort(c(
#         paste(
#           "Subset dictionary",
#           sv_el,
#           "has variables in item 'hesper_vars' that are not in param 'hesper_vars'"
#         ),
#         "*" = "Subset dictionary",
#         sv_el,
#         "has the following variables in item 'hesper_vars' that are not in param 'hesper_vars':",
#         paste0(hesper_vars_names, collapse = ", "),
#         "i" = "Item 'hesper_vars' should only contain variables that you inputed in param 'hesper_vars'."
#       ))
#     }

#     # sublist element "subset_var" is a character scalar and belong in df
#     checkmate::assertCharacter(sublist[["subset_var"]], len = 1, min.chars = 1)
#     if (!(sublist[["subset_var"]] %in% colnames(df))) {
#       rlang::abort(c(
#         paste(
#           "Subset dictionary",
#           sv_el,
#           "has variable in item 'subset_var' that is not in df"
#         ),
#         "*" = paste(
#           "Subset dictionary",
#           sv_el,
#           "has the following variable in item 'subset_var' that is not in df:",
#           sublist[["subset_var"]],
#           "."
#         ),
#         "i" = "Item 'subset_var' should only contain variables that are in df."
#       ))
#     }

#     # sublist element "subset_vals" is a character vector without NA or NULL values
#     checkmate::assertCharacter(
#       sublist[["subset_vals"]],
#       min.chars = 1,
#       null.ok = FALSE,
#       any.missing = FALSE
#     )

#     # warn if there is no occurence of "subset_vals" in "subset_var" in df
#     if (!any(sublist[["subset_vals"]] %in% df[[sublist[["subset_var"]]]])) {
#       notify(
#         paste0(
#           "For subset dictionary ",
#           sv_el,
#           " none of the following 'subset_vals' {",
#           paste0(
#             setdiff(sublist[["subset_vals"]], df[[sublist[["subset_var"]]]]),
#             collapse = "; "
#           ),
#           "} exist in the column 'subset_var' {",
#           sublist[["subset_var"]],
#           "} in df."
#         ),
#         warn = warn_subset_val_no_match
#       )
#     }
#   }
# }
