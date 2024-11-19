#' Are columns in dataset
#' 
#' This function checks if columns are present in the dataset and returns those that are not.
#' 
#' @param df
#' @param vars
#' 
#' @return
#' @export
#' 
check_vars_in <- function(df, vars) {

  #------ Checks 

  # df is a dataframe
  if (!is.data.frame(df)) rlang::abort("`df` should be a dataframe")
  
  # vars is a character vector
  if (!is.character(vars)) rlang::abort("`vars` should be a character vector")

  #------ Run 

  vars_nin <- vars[!vars %in% colnames(df)]

  if (length(vars_nin) > 0) {
    rlang::abort(
      c(
        "Some variables are not in `df`",
        "*" = "The following variables are not in `df`: {paste(vars_nin, collapse = ', ')}"
      )
    )
  }
}


#' Check values in a data frame that are not in a specified set
#'
#' This function checks the values of specified variables in a data frame that are not in a given set.
#'
#' @param df A data frame to check.
#' @param vars A character vector of variable names to check.
#' @param set A set of values to check against.
#'
#' @return A list of unique values for each variable that are not in the set.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(x = c(1, 2, 3, 4, 5), y = c("a", "b", "c", "d", "e"))
#'
#' # Define a set of values to check against
#' set <- c(1, 2, "a", "b")
#'
#' # Check values in the data frame that are not in the set
#' result <- check_values_in(df, c("x", "y"), set)
#' print(result)
check_values_in <- function(df, vars, set) {

  #------ Checks

  # vars in
  check_vars_in(df, vars)

  #------ Run
  
  # Get values for all vars that are not in set, sapply?
  nin_set_lgl <- sapply(df[vars], \(x) !all(unique(x) %in% set))

  return(nin_set_lgl)
}

