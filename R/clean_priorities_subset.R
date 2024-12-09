#' Clean top one/two/three priorities child binary columns with NA for subset of data 
#' 
#' This function replaces the top 1/2/3 binary columns (which correspond to choices) with `NA` for a specified subset of the data. 
#' 
#' @param data A data frame or data table
#' @param col_prio A character vector of length 3 with the names of the top 1/2/3 priority columns
#' @param subset_cols_vals A list of list with the names of the columns to subset the data and the values to subset the data
#' This list be a nested list with names of the subset columns as first layer, with as many names as subset dimension to clean for
#' For each subset dimension, another list with as many values than subset groups that needs to be cleaned with 
#' then two list elements linking to all values corresponding to the subset in data set followed by the choice value for the parent target columns that needs to be cleaned with NA for the corresponding subset   
#'
#' @return A data frame or data table with the top 1/2/3 priority columns replaced with `NA` for the specified subset of the data
#' @export
#'
#' @examples
#' 
#' hesper_dat_clean <- hesper_dat %>% 
#'   ## expand the top priorities columns to have binaries for each choice
#'   expand_bin(c("hesper_priority_first", "hesper_priority_second", "hesper_priority_third")) %>%
#'   ## clean the top priorities child columns for the subset of data (standard list of subset and choices)
#'   clean_top_priorities_subset(data = .,
#'                               col_prio = c("hesper_priority_first", "hesper_priority_second", "hesper_priority_third"))
#'
#' ## check that it did clean the parent columns correctly:
#' hesper_dat_clean %>% 
#'   filter(pop_group %in% c("idp", "refugees")) %>% select(pop_group, contains("displaced")) %>% distinct
#'

clean_top_priorities_subset <- function(
  data,
  col_prio=c("hesper_priority_first", "hesper_priority_second", "hesper_priority_third"),
  subset_cols_vals= list(
    "pop_group"=list("displaced"=list("subset_val"=c("idp", "refugees"), 
                                      "col_val"=c("hesper_displaced"))),
    "resp_gender"=list("female"=list("subset_val"=c("female"),
                                     "col_val"=c("hesper_clean_women")))
    ),
  sep.val="."
  ){
  ## if checkmate:: datatable not true, transform in DT
  if (!checkmate::testDataTable(data)){
    data <- data.table::as.data.table(data)
  }
  
  ## Check that subset_cols_vals is a list of list with non empty values 
  if (!all(map_lgl(subset_cols_vals, ~is_not_empty(.x)))) {
    stop("subset_cols_vals must be a list of list with non empty values")
  }
  
  ## check that the names of subset_cols_vals argument corresponding to the subset columns is a vector of character contained in data colnames.
  if (!all(names(subset_cols_vals) %in% colnames(data))) {
    stop(paste0("The following subset columns are not in the data: ", paste(names(subset_cols_vals) %>% setdiff(colnames(data)), collapse = ", ")))
  }
  
  ## Check the values of each of the list name is contained in the unique values of data[[names(list)]]
  if (!all(map_lgl(names(subset_cols_vals), ~all(unlist(map(subset_cols_vals[[.x]], ~.[["subset_val"]])) %in% unique(data[[.x]]))))){
    stop("The values for each subset category must be contained in the unique values of corresponding subset column.")
  }
  
  ## Check that the col_prio are in data, or stop and print colnames
  if (!all(col_prio %in% colnames(data))) {
    col_not_in_data <- col_prio %>% setdiff(colnames(data))
    stop(paste0("The following target priority parent columns to be cleaned are not in the data: ", paste(col_not_in_data, collapse = ", ")))
  }
  
  ## Check that subset_cols_vals list, any of the list of list of list elements which names are "col_val" are existing choices in the col_prio (in data[[col_prio]] unique values)
  if (!all(unlist(map_depth(subset_cols_vals, 2, ~ .x$col_val)) %in% unique(unlist(data[, .SD, .SDcols = col_prio])))){
    stop("The values of  associated with each subset category must be contained in the unique values of the column corresponding to subset_cols_vals names")
  }
  
  ## for each element of subset_cols_vals list of arguments, replace_na_subset with for the relevant subset col and values
  for (subset_col in names(subset_cols_vals)){
    for (subset_val in names(subset_cols_vals[[subset_col]])){
      ## print a message to display that for the corresponding subset_col and subset_val subset_cols_vals[[subset_col]][[subset_val]][["subset_val"]]
      ## the col_prio child binary columns for the following choices subset_cols_vals[[subset_col]][[subset_val]][["col_val"]] will be replaced with NA
      message(paste0("Cleaning the top priorities child columns for the subset of data: ", subset_col, " = ", subset_cols_vals[[subset_col]][[subset_val]][["subset_val"]], 
                     " for the following hesper choices: ", subset_cols_vals[[subset_col]][[subset_val]][["col_val"]]))
      data <- replace_na_subset(data,
                                subset_col = subset_col,
                                subset_value = subset_cols_vals[[subset_col]][[subset_val]][["subset_val"]],
                                col_parent = col_prio,
                                choice_vals = subset_cols_vals[[subset_col]][[subset_val]][["col_val"]],
                                sep = sep.val)

    }
  }
  
  return(data)
}


