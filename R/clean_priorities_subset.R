# #' Clean top one/two/three priorities child binary columns with NA for subset of data
# #'
#' This function replaces the top 1/2/3 binary columns (which correspond to choices) with `NA` for a specified subset of the data.
#'
#' @param data A data frame or data table
#' @param col_prio A character vector of length 3 with the names of the top 1/2/3 priority columns
#' @param subset_cols_vals A list of list with the names of the columns to subset the data and the values to subset the data
#' This list be a nested list with names of the subset columns as first layer, with as many names as subset dimension to clean for
#' For each subset dimension, another list with as many values than subset groups that needs to be cleaned with
#' then two list elements linking to all values corresponding to the subset in data set followed by the choice value for the parent target columns that needs to be cleaned with NA for the corresponding subset
#' @param sep_val A character scalar as the separator used in the child binary column names
#' @param choice_suffix_subset A character as the suffix used for the child binary column names cleaned for subset
#'
#' @return A data frame or data table with the top 1/2/3 priority columns replaced with `NA` for the specified subset of the data
#' @export
#'
# #' @examples
# #'
# #' ## Expand the top first second and third priority columns into child binaries and then clean the top priorities child columns for the subset of data
# #' hesper_dat_clean <- hesper_dat %>%
# #'   ## expand the top priorities columns to have binaries for each choice
# #'   expand_bin(c("hesper_priority_first", "hesper_priority_second", "hesper_priority_third")) |>
# #'   ## clean the top priorities child columns for the subset of data (standard list of subset and choices)
# #'   clean_top_priorities_subset(data = .,
# #'                               col_prio = c("hesper_priority_first", "hesper_priority_second", "hesper_priority_third"))
#'
# #' ## check that it did clean the parent columns correctly:
# #' hesper_dat_clean |>
# #'   filter(pop_group %in% c("idp", "refugees")) |> dplyr::select(pop_group, dplyr::contains("displaced")) |> dplyr::distinct()
#'
clean_top_priorities_subset <- function(
  df,
  col_prio = c(
    "hesper_priority_first",
    "hesper_priority_second",
    "hesper_priority_third"
  ),
  sv_l_val = list(
    displaced = list(
      hesper_vars = c("hesper_displaced"),
      subset_var = "pop_group",
      subset_vals = c("refugees", "idp")
    ),
    resp_gender_female = list(
      hesper_vars = c("hesper_clean_female"),
      subset_var = "resp_gender",
      subset_vals = c("female")
    )
  ),
  sep_val = ".",
  choice_suffix_subset = NULL
) {
  ## if checkmate:: datatable not true, transform in DT
  if (!checkmate::testDataTable(df)) {
    df <- data.table::as.data.table(df)
  }

  ## Check that the col_prio are in df
  check_vars_in_df(df, col_prio)

  # sv_l_val is a named list
  checkmate::assertList(sv_l_val, names = "unique")

  # sv_l_val items has three items only that are named hesper_vars, subset_var and subset_vals:
  check_sv_l(sv_l_val, df, hesper_vars)

  # replace_na_subset for each sv_l_val as above with the new list structure to clean chid binary for all relevant subset
  for (i in seq_along(sv_l_val)) {
    sv <- sv_l_val[[i]]
    name_subset <- names(sv_l_val)[i]

    # get colnames in df
    col_prio_sub <- paste0(col_prio, sep_val, sv$hesper_vars)
    col_prio_sub_in_data <- col_prio_sub[col_prio_sub %in% names(df)]

    if (length(col_prio_sub_in_data) > 0) {
      message(paste0(
        "Cleaning the top priorities child columns for the subset '",
        name_subset,
        "' corresponding to: ",
        sv$subset_var,
        " in '",
        paste0(sv$subset_vals, collapse = "', '"),
        "' for the following hesper choices: ",
        sv$hesper_vars
      ))

      # retrieve choices in df
      ## if sep is special character, escape it with \\ (use checkmate)
      special_chars <- "[\\.\\|\\(|\\)|\\[|\\]|\\{|\\}|\\^|\\$|\\*|\\+|\\?|\\\\|\\.]"
      if (grepl(special_chars, sep_val)) {
        sep.escaped <- paste0("\\", sep_val)
      }
      choice_prio_in_data <- gsub(
        paste0(paste0(col_prio, sep.escaped), collapse = "|"),
        "",
        col_prio_sub_in_data
      ) |>
        unique()

      ## create a new column paste0(col_prio, sep, choice_vals, choice_suffix_subset) with the subset suffix to be cleaned
      if (!is.null(choice_suffix_subset)) {
        df[,
          paste0(col_prio_sub_in_data, choice_suffix_subset) := df[,
            col_prio_sub_in_data,
            with = F
          ]
        ]
      }

      df <- replace_na_subset(
        df,
        subset_col = sv$subset_var,
        subset_value = sv$subset_vals,
        col_parent = col_prio,
        choice_vals = paste0(choice_prio_in_data, choice_suffix_subset),
        sep = sep_val
      )
    } else {
      message(paste0(
        "No top priorities child columns found for the subset '",
        name_subset,
        "' corresponding to: ",
        sv$subset_var,
        " in '",
        paste0(sv$subset_vals, collapse = "', '"),
        "' for the following hesper choices: ",
        sv$hesper_vars,
        ".\nNo cleaning done as no matching child columns."
      ))
    }
  }

  return(df)
}
