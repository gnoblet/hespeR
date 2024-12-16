#' Expand a variable into binary indicators
#'
#' This function takes a dataframe and a variable, and expands it into binary indicators.
#' The variable is split by the `split_by` separator, and each choice is represented by a binary column.
#' The binary columns are separated by the `bin_sep` separator.
#'
#' @param df The input dataframe.
#' @param vars The name of the variables to expand.
#' @param split_by The separator used to split the variable into choices (default: " ").
#' @param bin_sep The separator used to separate the original variable name and the choice name in the binary columns (default: ".").
#' @param drop_undefined A character vector of values to consider as undefined. Defaults to NULL if none.
#' @param value_in A character vector of values to consider as value_in. Defaults to NULL if none.
#' @param value_in_suffix A character scalar or an empty string to append to the variable names. Defaults to NULL.
#' @param remove_new_bin A logical scalar indicating whether to remove the new binary columns if they already exist in the dataframe. Defaults to TRUE.
#' @param remove_other_bin A logical scalar indicating whether to remove other binary columns starting with the variable name and the bin_sep. Defaults to TRUE.
#' @return The modified dataframe with as many binary columns as there are choices in the original variable.
#' 
#' @importFrom data.table `.SD`
#' @importFrom data.table `:=`
#' @importFrom data.table `.I`
#' 
#' @examples
#' df <- data.frame(var1 = c("a b c", "a c", "d", NA), var2 = c("a b c", "a c", "c a", NA))
#' df <- expand_bin(df, c("var1", "var2"))
#' df
#' 
#' @export
expand_bin <- function(df, vars, split_by = " ", bin_sep = ".", drop_undefined = NULL, value_in = NULL, value_in_suffix = NULL, remove_new_bin = TRUE, remove_other_bin = TRUE) {

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

  # all vars are in df and of class character
  for (var in vars) {
    if (!(var %in% colnames(df))) rlang::abort(paste0("Variable ", var, " not found in df."))
    checkmate::assertClass(df[[var]], "character", .var.name = var)
  }

  # drop_undefined is a character vector or NULL
  checkmate::assertCharacter(drop_undefined, null.ok = TRUE)

  # value_in is a character vector or NULL
  checkmate::assertCharacter(value_in, null.ok = TRUE)

  # value_in_suffix is a character scalar or an empty string or NULL
  checkmate::assertCharacter(value_in_suffix, len = 1, null.ok = TRUE)

  # remove_new_bin is a logical scalar
  checkmate::assertLogical(remove_new_bin, len = 1)

  # remove_other_bin is a logical scalar
  checkmate::assertLogical(remove_other_bin, len = 1)

  #------ Do stuff
    

  # Create a unique id for each row 
  df[, 'key_id' := .I]

  for (var in vars) {

    # Step 1: Create binary indicators for each choice in `var`
      
    # Drop undefined
    if (!is.null(drop_undefined)) {
      df.bin <- drop_undefined(df, var, vals_undefined = drop_undefined)
    } else {
      df.bin <- df
    }

    # Split longer (removing NAs) 
    df.bin <- df.bin[
      !is.na(get(var)), 
      list(temp = unlist(strsplit(get(var), split_by, fixed = TRUE))), 
      by = "key_id"]
    
    # Remove duplicates
    df.bin <- unique(df.bin, by = c("key_id", "temp"))
      
    # Reshape to wide format, filling missing values with 0
    df.bin <- dcast(df.bin, key_id ~ paste0(var, bin_sep, temp), value.var = "temp", fun.aggregate = length, fill = 0)
      
    # Get new binary column names
    names.bin <- setdiff(colnames(df.bin), "key_id")
    
    if (remove_new_bin){
      # Remove from df the names.bin binary columns if they exist, and warn for replacement
      names.bin_replace <- names.bin[names.bin %in% colnames(df)]
      if (length(names.bin_replace) > 0){
        warn_replace(df, names.bin)
        df[, (names.bin_replace) := NULL]
      }
    }
      
    if (remove_other_bin){
      # Also remove all variables starting with "varbinsep" to avoid confusion
      names.bin_removal <- colnames(df)[startsWith(colnames(df), paste0(var, "."))]
      if (length(names.bin_removal) > 0){
        warn_removal(df, names.bin_removal)
        df[, (names.bin_removal) := NULL]
      }
    }
      
    # Intermediate step - value in
    if (!is.null(value_in)) {

      names.bin.value_in <- paste0(var, bin_sep, value_in)
      
      # if this binary variables does not exist, create it and add NAs when var is NA
      for (name in names.bin.value_in) {

        if (!(name %in% colnames(df.bin))) {
          df.bin[, (name) := ifelse(is.na(get(var)), NA, 0)]
        }
      }

      # if any of names.bin.value_in is 1, then add 1 to a new variable 
      if (is.null(value_in_suffix)) {
        name.bin.suffix <- paste0(var, ".", "value_in")
      } else {
        name.bin.suffix <- paste0(var, ".", value_in_suffix)
      } 
      df.bin[, (name.bin.suffix) := data.table::fifelse(rowSums(.SD) > 0, 1, 0), .SDcols = names.bin.value_in]

    }
    
    # Step 2: Merge back the binary columns to the original dataframe and remove row_id
    df <- merge(df, df.bin, by = "key_id", all.x = TRUE)
    
    # Step 3: Reorder columns to place binary columns after `var`
    data.table::setcolorder(df, c(setdiff(colnames(df), c(var, names.bin)), var, names.bin))
    # reorder value_in columns
    if (!is.null(value_in)) {
      data.table::setcolorder(df, c(setdiff(colnames(df), c(var, names.bin, name.bin.suffix)), var, names.bin, name.bin.suffix))
    }

  }

  # Step 4: Remove key_id
  df[, 'key_id' := NULL]

  return(df)
}
