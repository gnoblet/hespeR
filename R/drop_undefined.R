#' Drop undefined values in a data frame
#'
#' This function drops undefined values in a data frame by replacing them with NA.
#'
#' @param df A data frame to modify.
#' @param vars A character vector of variable names to check for undefined values.
#' @param vals_undefined A character vector of values to consider as undefined. Defaults to c("pnta", "dnk").
#' @param suffix A character scalar or an empty string to append to the variable names. Defaults to NULL.
#'
#' @return The modified data frame with undefined values replaced by NA. If suffix is NULL or an empty string, variables vars are modified; otherwise, new variables are added with suffix.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.table::data.frame(x = c("a", "b", "pnta", "dnk", "e"), y = c("a", "b", "c", "d", "e"))
#'
#' # Define variables and undefined values
#' vars <- c("x", "y")
#' vals_undefined <- c("pnta", "dnk")
#'
#' # Drop undefined values
#' result <- drop_undefined(df, vars, vals_undefined)
#' print(result)
#' 
#' @export 
 drop_undefined <- function(df, vars, vals_undefined = c("pnta", "dnk"), suffix = NULL){

  #------ Checks

  # df is a dataframe
  checkmate::assertDataFrame(df)

  # df is not data.table, convert it
  if (!checkmate::testDataTable(df)) {
    rlang::warn("Converting df to data.table.")
    data.table::setDT(df)
  }

  # vars is a character vector
  checkmate::assertCharacter(vars, min.chars = 1)

  # vals_undefined is a character vector
  checkmate::assertCharacter(vals_undefined, min.chars = 1)

  # all vars are in df and of class character
  for (var in vars) {
    if (!(var %in% colnames(df))) rlang::abort(paste0("Variable ", var, " not found in df."))
    checkmate::assertClass(df[[var]], "character", .var.name = var)
  }

  # suffix is a character scalar or an empty string
  checkmate::assertCharacter(suffix, len = 1, null.ok = TRUE)

  #------ Do stuff

  # Prepare newnames with suffix
  vars_suffix <- paste0(vars, suffix)

  # Drop undefined values
  df[,(vars_suffix) := lapply(.SD, \(x) ifelse(x %in% vals_undefined, NA_character_, x))
     , .SDcols = vars]

  return(df)
 
}