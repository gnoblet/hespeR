#' Expand a variable into binary indicators
#'
#' This function takes a dataframe and a variable, and expands it into binary indicators.
#' The variable is split by the `split_by` separator, and each choice is represented by a binary column.
#' The binary columns are separated by the `bin_sep` separator.
#'
#' @param df The input dataframe
#' @param var The name of the variable to expand
#' @param split_by The separator used to split the variable into choices (default: " ")
#' @param bin_sep The separator used to separate the original variable name and the choice name in the binary columns (default: ".")
#'
#' @return The modified dataframe with as many binary columns as there are choices in the original variable.
#' 
#' @examples
#' df <- data.frame(var1 = c("a b c", "a c", "d", NA), var2 = c("a b c", "a c", "c a", NA))
#' df <- expand_bin(df, c("var1", "var2"))
#' df
#' 
#' @export
expand_bin <- function(df, vars, split_by = " ", bin_sep = ".") {

  #------ Checks

  # df is a dataframe
  checkmate::assertDataFrame(df)

  # if df is not data.table, convert it
  if (!checkmate::testDataTable(df)) {
    rlang::warn("Converting df to data.table.")
    data.table::setDT(df)
  }

  # split_by is character of length 1
  checkmate::assertCharacter(split_by, len = 1, min.chars = 1)

  # bin_sep is character of length 1
  checkmate::assertCharacter(bin_sep, len = 1, min.chars = 1)

  # vars is a character vector 
  checkmate::assertCharacter(vars, min.chars = 1)

  # Check if all vars are in df and of class character
  for (var in vars) {
    if (!(var %in% colnames(df))) rlang::abort(paste0("Variable ", var, " not found in df."))
    checkmate::assertClass(df[[var]], "character", .var.name = var)
  }

  #------ Compute
    

  # Create a unique id for each row 
  df[, key_id := .I]

  for (var in vars) {

  # Step 1: Create binary indicators for each choice in `var`

  # Split longer (removing NAs) 
  df.bin <- df[
    !is.na(get(var)), 
    .(temp = unlist(strsplit(get(var), split_by, fixed = TRUE))), 
    by = "key_id"]
  
  # Remove duplicates
  df.bin <- unique(df.bin, by = c("key_id", "temp"))
    
  # Reshape to wide format, filling missing values with 0
  df.bin <- dcast(df.bin, key_id ~ paste0(var, bin_sep, temp), value.var = "temp", fun.aggregate = length, fill = 0)
    
  # Get new binary column names
  names.bin <- setdiff(colnames(df.bin), "key_id")
  
  # Remove from df the names.bin binary columns if they exist, and warn for replacement
  warn_replace(df, names.bin)
  df[, (names.bin[names.bin %in% colnames(df)]) := NULL]
  
  
  # Step 2: Merge back the binary columns to the original dataframe and remove row_id
  df <- merge(df, df.bin, by = "key_id", all.x = TRUE)
  
  # Step 3: Reorder columns to place binary columns after `var`
  data.table::setcolorder(df, c(setdiff(colnames(df), c(var, names.bin)), var, names.bin))
  }

  # Step 4: Remove key_id
  df[, key_id := NULL]

  return(df)
}
