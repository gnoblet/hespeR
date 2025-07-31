#' Replace binary columns with NA for a subset of the data
#'
#' This function replaces binary columns (which correspond to choices) with `NA` for a specified subset of the data.
#' It is typically used when expanding top priority columns into child binary columns, ensuring that child columns for
#' choices that do not apply to a subset are set to `NA`.
#'
#' @typed df:
#'   A data.table or data.frame: The data to be processed.
#' @typed subset_col:
#'   A character vector: The name of the column that defines the subset.
#' @typed subset_value:
#'   A character vector: The value in `subset_col` for which binary columns will be replaced with `NA`.
#' @typed sep:
#'   A character vector: The separator used in the child binary column names (default is ".").
#' @typed col_parent:
#'   A character vector: The name of the parent column (usually the question) that defines child binary columns.
#' @typed choice_vals:
#'   A character vector: The values corresponding to the child binary columns that should be replaced with `NA`.
#'
#' @typedreturn
#'   A data.table or data.frame with the binary columns replaced by `NA` for the relevant subset.
#'
# #' @examples
# #' # Create a fake dataset
# #' subset_col <- "pop_group"
# #' subset_values <- c("displaced", "host")
# #' sep <- "."
# #' col_parent <- "hesper_top_three_priorities"
# #' choice_vals <- c("hesper_displaced", "hesper_food", "hesper_water", "hesper_info_displaced")
# #' df <- data.frame(
# #'   pop_group = sample(subset_values, 100, replace = TRUE),
# #'   hesper_top_three_priorities = sample(c("hesper_displaced", "hesper_food", "hesper_water"), 100, replace = TRUE),
# #'   hesper_top_three_priorities.hesper_displaced = sample(0:1, 100, replace = TRUE),
# #'   hesper_top_three_priorities.hesper_food = sample(0:1, 100, replace = TRUE),
# #'   hesper_top_three_priorities.hesper_water = sample(0:1, 100, replace = TRUE),
# #'   hesper_top_three_priorities.hesper_info_displaced = sample(0:1, 100, replace = TRUE)
# #' )
# #'
# #' # Use the replace_na_subset function
# #' df %>%
# #'   replace_na_subset(subset_col = "pop_group",
# #'                     subset_value = "displaced",
# #'                     sep = ".",
# #'                     col_parent = "hesper_top_three_priorities",
# #'                     choice_vals = c("hesper_displaced", "hesper_info_displaced", "cleaning_displaced"))
replace_na_subset <- function(
  df,
  subset_col,
  subset_value,
  sep = ".",
  col_parent,
  choice_vals
) {
  #------ Checks

  # if checkmate:: datatable not true, transform in DT
  if (!checkmate::testDataTable(df)) {
    df <- data.table::as.data.table(df)
  }

  # subset_col and col_parent are in df, or stop and print colnames
  check_vars_in_df(df, c(subset_col, col_parent))

  ## check that subset_value is in subset_col, or stop and print unique values
  if (!any(subset_value %in% unique(df[[subset_col]]))) {
    unique_values <- unique(df[[subset_col]])
    rlang::abort(paste0(
      "The value ",
      subset_value,
      " is not in the column ",
      subset_col,
      ". Unique values are: ",
      paste(unique_values, collapse = ", ")
    ))
  }

  ## if sep is special character, escape it with \\ (use checkmate)
  special_chars <- "[\\.\\|\\(|\\)|\\[|\\]|\\{|\\}|\\^|\\$|\\*|\\+|\\?|\\\\|\\.]"
  if (grepl(special_chars, sep)) {
    sep.escaped <- paste0("\\", sep)
  }

  ## check that choice separator is the relevant one
  all.cols <- purrr::map(col_parent, \(x) {
    colnames(df)[grepl(paste0(x, sep.escaped), colnames(df))]
  }) |>
    unlist()

  if (length(all.cols) == 0) {
    rlang::abort(paste0(
      "No child columns with the parent column ",
      col_parent,
      " and choice separator ",
      sep,
      " were found in df."
    ))
  }
  ## if not all choice_vals in df, stop and print the missing cols
  missing_cols <- paste0(col_parent, sep, choice_vals) |> setdiff(all.cols)
  if (length(missing_cols) > 0) {
    rlang::abort(paste0(
      "The following child columns are missing: ",
      paste(missing_cols, collapse = ", ")
    ))
  }

  ## replace binary columns with NA for the subset of the df if there is a match
  cols.subset <- purrr::map(col_parent, \(x) {
    colnames(df)[grepl(
      paste0(x, sep.escaped, "(", paste0(choice_vals, collapse = "|"), ")"),
      colnames(df)
    )]
  }) |>
    unlist()
  df[get(subset_col) %in% subset_value, (cols.subset) := NA]

  return(df)
}
