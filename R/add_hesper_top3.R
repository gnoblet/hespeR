#' @title function that compute for each respondent the number of selected items, number of applicable items and collapse the three ordered priorities into one select multiple column
#'
#' @description The function computes the number of selected items for each respondent, the number of applicable items and collapse the three ordered priorities into one select multiple column
#' @param df dataframe containing the cleaned data
#' @param col_items vector of column names containing the hesper items.
#' @param choice_serious character string representing the choice for serious problem
#' @param choice_no_serious character string representing the choice for no serious problem
#' @param choice_dnk character string representing the choice for do not know
#' @param choice_pnta character string representing the choice for decline to answer
#' @param choice_na character string representing the choice for not applicable to household
#' @param vars_priority vector of three column names containing the first, second and third priority hesper items
#' @param col_hesper_top_three character string representing the column name containing the three ordered priorities
#' @param col_gender character string representing the column name containing the respondent's gender
#' @param choices_male character string representing the choice corresponding to male respondent gender
#' @param choices_female character string representing the choice corresponding to female respondent gender
#' @param hesper_item_male vector of two choice names corresponding to the hesper items specific for male respondent
#' @param hesper_item_female vector of two choice names corresponding to the hesper items specific for female respodent
#' @param col_displacement character string representing the column name containing the respondent's displacement status
#' @param choices_displaced character string representing the choice corresponding to displaced respondent
#' @param choices_non_displaced character string representing the choice corresponding to not displaced respondent
#' @param hesper_item_displaced vector of two choices name coresponding to the hesper items specific for displaced respondent
#' @param hesper_item_non_displaced vector of two choices name coresponding to the hesper items specific for not displaced respondent
#' @param add_binaries logical value indicating whether to add binary variables for each hesper item
#' @param add_binaries_subset logical value indicating whether to add binary variables for each hesper item that are applicable to only a subset of the population
#' @param subset logical value indicating whether to add binary subset variables for all top three priority child columns that are applicable to only a subset of the population
#' @param add_binaries_undefined logical value indicating whether to add binary variables for each hesper item that are applicable to only a subset of the population
#'
#' @return dataframe with the number of selected items, the number of applicable items and the three ordered priorities collapsed into one select multiple column
#'
#' @export
#'
add_hesper_top3 <- function(df,
                            vars_priority = c("hesper_priority_first",
                                              "hesper_priority_second",
                                              "hesper_priority_third"),
                            var_hesper_top_three="hesper_top_three_priorities",
                            col_gender = "gender",
                            choices_male = "men",
                            choices_female = "women",
                            hesper_item_male = NULL,
                            hesper_item_female = c("hesper_clean_women"),
                            col_displacement = "pop_group",
                            choices_displaced = c("refugees", "idp"),
                            choices_non_displaced = c("hosts"),
                            hesper_item_displaced = c("hesper_displaced"),
                            hesper_item_non_displaced = NULL
){

  #------ Checks

  # df is a dataframe
  checkmate::assertDataFrame(df)

  # df is not data.table, convert it
  if (!checkmate::testDataTable(df)) {
    rlang::warn("Converting df to data.table.")
    data.table::setDT(df)
  }

  ## check that vars_priority is character vector
  checkmate::assert_character(vars_priority)

  # hesper_vars are unique
  check_dupes(vars_priority, "The following vars are duplicated: ")

  # check that there are three vars in vars_priority
  if (length(hesper_vars) != 3) {
    rlang::abort("vars_priority should contain three variables names.")
  }

  # all vars are in df
  check_vars_in_df(vars_priority)

  # hesper_vars are character vars in df
  check_vars_class_in_df(df, vars_priority, "character")

  # check that col_gender col_displacement are character
  checkmate::assert_character(c(col_gender, col_displacement))

  # check that c(choices_male, choices_female) are character
  checkmate::assert_character(c(choices_male, choices_female, choices_displaced, choices_non_displaced))

  # check that c(choices_male, choices_female) are contained in df in col col_gender
  check_vars_in_set(df, col_gender, c(choices_male, choices_female))

  # check that c(choices_displaced, choices_non_displaced) are contained in df in col_displacement
  check_vars_in_set(df, col_displacement, c(choices_displaced, choices_non_displaced))

  # check that hesper_item_female, hesper_item_male, hesper_item_displaced, hesper_item_non_displaced are character
  checkmate::assert_character(c(hesper_item_female, hesper_item_male, hesper_item_displaced, hesper_item_non_displaced))

  # check that hesper_item_female, hesper_item_male, hesper_item_displaced, hesper_item_non_displaced are in df
  check_vars_in_df(df, c(hesper_item_female, hesper_item_male, hesper_item_displaced, hesper_item_non_displaced))

  ## collapse priority columns to have a select multiple column, create dummy binary child columns (with & without subset) & ensure that skip logic is respected for top three binaries

  ## unite the thre priority columns to have one select multiple hesper priorities
  df <-  add_top_three(df, new_var = col_hesper_top_three, vars_unite = vars_priority)

  ### expand parent column top three priorities and priority without accounting for subset
  df <- expand_bin(df, c(col_hesper_top_three))

  ### ensure that skip logic are respected for top three to avoid having binaries with zero for items that are not applicable to the respondent

  # mutate all subset binaries with _subset at the end before reworking them, only for subset present in the function's arguments
  col_hesper_male <- if (is_not_empty(hesper_item_male)) paste0(col_hesper_top_three, ".", hesper_item_male, "_subset") else NULL
  col_hesper_female <- if (is_not_empty(hesper_item_female)) paste0(col_hesper_top_three, ".", hesper_item_female, "_subset") else NULL
  col_hesper_displaced <- if (is_not_empty(hesper_item_displaced)) paste0(col_hesper_top_three, ".", hesper_item_displaced, "_subset") else NULL
  col_hesper_non_displaced <- if (is_not_empty(hesper_item_non_displaced)) paste0(col_hesper_top_three, ".", hesper_item_non_displaced, "_subset") else NULL
  col_hesper_subset <- c(col_hesper_male, col_hesper_female, col_hesper_displaced, col_hesper_non_displaced)
  col_hesper_item_subset <- col_hesper_subset |> stringr::str_replace_all("_subset$", "")

  ## create new variables with _subset name suffix for any match of col_hesper_item_subset with data.table syntax
  df[, paste0(col_hesper_item_subset, "_subset") := .SD, .SDcols = col_hesper_item_subset]

  # replace any male specific top three priority child columns with NA for relevant respondents
  if (is_not_empty(hesper_item_male)) {
    df <- replace_na_subset(
      df,
      subset_col = col_gender,
      subset_value = choices_male,
      sep = ".",
      col_parent = col_hesper_top_three,
      choice_vals = paste0(hesper_item_male, "_subset")
    )
  }

  # replace any female specific top three priority child columns with NA for relevant respondents
  if (is_not_empty(hesper_item_female)) {
    df <- replace_na_subset(
      df,
      subset_col = col_gender,
      subset_value = choices_female,
      sep = ".",
      col_parent = col_hesper_top_three,
      choice_vals = paste0(hesper_item_female, "_subset")
    )
  }

  # replace any displaced specific top three priority child columns with NA for relevant respondents
  if (is_not_empty(hesper_item_displaced)) {
    df <- replace_na_subset(
      df,
      subset_col = col_displacement,
      subset_value = choices_displaced,
      sep = ".",
      col_parent = col_hesper_top_three,
      choice_vals = paste0(hesper_item_displaced, "_subset")
    )
  }

  # replace any non displaced specific top three priority child columns with NA for relevant respondents
  if (is_not_empty(hesper_item_non_displaced)) {
    df <- replace_na_subset(
      df,
      subset_col = col_hesper_non_displaced,
      subset_value = choices_non_displaced,
      sep = ".",
      col_parent = col_hesper_top_three,
      choice_vals = paste0(hesper_item_non_displaced, "_subset")
    )
  }

  return(df)

}

