## unite the thre priority columns to have one select multiple hesper priorities

#' Add a new column with the top three priorities
#' @param df input dataframe
#' @param new_col name of the new column
#' @param cols_unite vector with column names corresponding to top 1/2/3 priority to combine in unordered top three priorities
#' @return dataframe with new column
#' @export
#' 

add.top.three <- function(df, new_col, cols_unite){
  df <- df %>%
    unite(!!sym(new_col), all_of(cols_unite), sep = " ", remove = F, na.rm = T) %>%
    mutate(!!sym(new_col) := ifelse(!!sym(new_col)=="", NA, str_replace_all(!!sym(new_col), "^ | $", "")))
}

