# #' Expand a variable into binary indicators
# #'
# #' This function takes a dataframe and a variable, and expands it into binary indicators.
# #' The variable is split by the `split_by` separator, and each choice is represented by a binary column.
# #' The binary columns are separated by the `bin_sep` separator.
# #'
# #' @typed df:
# #'   The input dataframe.
# #' @typed vars:
# #'   The name of the variables to expand.
# #' @typed split_by:
# #'   The separator used to split the variable into choices (default: " ").
# #' @typed bin_sep:
# #'   The separator used to separate the original variable name and the choice name in the binary columns (default: ".").
# #' @typed drop_undefined:
# #'   A character vector of values to consider as undefined. Defaults to NULL if none.
# #' @typed value_in:
# #'   A character vector of values to consider as value_in. Defaults to NULL if none.
# #' @typed value_in_suffix:
# #'   A character scalar or an empty string to append to the variable names. Defaults to NULL.
# #' @typed remove_new_bin:
# #'   A logical scalar indicating whether to remove the new binary columns if they already exist in the dataframe. Defaults to TRUE.
# #' @typed remove_other_bin:
# #'   A logical scalar indicating whether to remove other binary columns starting with the variable name and the bin_sep. Defaults to TRUE.
# #' @typedreturn
# #'   The modified dataframe with as many binary columns as there are choices in the original variable.
# #'
# #' @importFrom data.table `.SD`
# #' @importFrom data.table `:=`
# #' @importFrom data.table `.I`
# #'
# #' @examples
# #' df <- data.frame(var1 = c("a b c", "a c", "d", NA), var2 = c("a b c", "a c", "c a", NA))
# #' df <- expand_bin(df, c("var1", "var2"))
# #' df
# #'
# #' @export
# expand_bin <- function(
#   df,
#   vars,
#   split_by = " ",
#   bin_sep = ".",
#   drop_undefined = NULL,
#   value_in = NULL,
#   value_in_suffix = NULL,
#   remove_new_bin = TRUE,
#   remove_other_bin = TRUE
# ) {
#   #------ Checks

#   # df is a dataframe
#   checkmate::assertDataFrame(df)

#   # if df is not data.table, convert it
#   if (!checkmate::testDataTable(df)) {
#     rlang::warn("Converting df to data.table.")
#     data.table::setDT(df)
#   }

#   # split_by is character of length 1
#   checkmate::assertCharacter(split_by, len = 1, min.chars = 1)

#   # bin_sep is character of length 1
#   checkmate::assertCharacter(bin_sep, len = 1, min.chars = 1)

#   # vars is a character vector
#   checkmate::assertCharacter(vars, min.chars = 1)

#   # all vars are in df
#   check_vars_in_df(df, vars)

#   # all vars are of class character
#   check_vars_class_in_df(df, vars, "character")

#   # drop_undefined is a character vector or NULL
#   checkmate::assertCharacter(drop_undefined, null.ok = TRUE)

#   # value_in is a character vector or NULL
#   checkmate::assertCharacter(value_in, null.ok = TRUE)

#   # value_in_suffix is a character scalar or an empty string or NULL
#   checkmate::assertCharacter(value_in_suffix, len = 1, null.ok = TRUE)

#   # remove_new_bin is a logical scalar
#   checkmate::assertLogical(remove_new_bin, len = 1)

#   # remove_other_bin is a logical scalar
#   checkmate::assertLogical(remove_other_bin, len = 1)

#   #------ Do stuff

#   # Create a unique id for each row
#   df[, 'key_id' := .I]

#   for (var in vars) {
#     # Step 1: Create binary indicators for each choice in `var`

#     # Drop undefined
#     if (!is.null(drop_undefined)) {
#       df_bin <- drop_undefined(df, var, vals_undefined = drop_undefined)
#     } else {
#       df_bin <- df
#     }

#     # Split longer (removing NAs)
#     df_bin <- df_bin[
#       !is.na(get(var)),
#       list(temp = unlist(strsplit(get(var), split_by, fixed = TRUE))),
#       by = "key_id"
#     ]

#     # Remove duplicates
#     df_bin <- unique(df_bin, by = c("key_id", "temp"))

#     # Reshape to wide format, filling missing values with 0
#     df_bin <- data.table::dcast(
#       df_bin,
#       key_id ~ paste0(var, bin_sep, temp),
#       value.var = "temp",
#       fun.aggregate = length,
#       fill = 0
#     )

#     # Get new binary column names
#     names_bin <- setdiff(colnames(df_bin), "key_id")

#     if (remove_new_bin) {
#       # Remove from df the names_bin binary columns if they exist, and warn for replacement
#       names_bin_replace <- names_bin[names_bin %in% colnames(df)]
#       if (length(names_bin_replace) > 0) {
#         warn_replace(df, names_bin)
#         df[, (names_bin_replace) := NULL]
#       }
#     }

#     if (remove_other_bin) {
#       # Also remove all variables starting with "varbinsep" to avoid confusion
#       names_bin_removal <- colnames(df)[startsWith(
#         colnames(df),
#         paste0(var, ".")
#       )]
#       if (length(names_bin_removal) > 0) {
#         warn_removal(df, names_bin_removal)
#         df[, (names_bin_removal) := NULL]
#       }
#     }

#     # Intermediate step - value in
#     if (!is.null(value_in)) {
#       names_bin.value_in <- paste0(var, bin_sep, value_in)

#       # if this binary variables does not exist, create it and add NAs when var is NA
#       for (name in names_bin.value_in) {
#         if (!(name %in% colnames(df_bin))) {
#           df_bin[, (name) := ifelse(is.na(get(var)), NA, 0)]
#         }
#       }

#       # if any of names_bin.value_in is 1, then add 1 to a new variable
#       if (is.null(value_in_suffix)) {
#         name.bin.suffix <- paste0(var, ".", "value_in")
#       } else {
#         name.bin.suffix <- paste0(var, ".", value_in_suffix)
#       }
#       df_bin[,
#         (name.bin.suffix) := data.table::fifelse(rowSums(.SD) > 0, 1, 0),
#         .SDcols = names_bin.value_in
#       ]
#     }

#     # Step 2: Merge back the binary columns to the original dataframe and remove row_id
#     df <- merge(df, df_bin, by = "key_id", all.x = TRUE)

#     # Step 3: Reorder columns to place binary columns after `var`
#     data.table::setcolorder(
#       df,
#       c(setdiff(colnames(df), c(var, names_bin)), var, names_bin)
#     )
#     # reorder value_in columns
#     if (!is.null(value_in)) {
#       data.table::setcolorder(
#         df,
#         c(
#           setdiff(colnames(df), c(var, names_bin, name.bin.suffix)),
#           var,
#           names_bin,
#           name.bin.suffix
#         )
#       )
#     }
#   }

#   # Step 4: Remove key_id
#   df[, 'key_id' := NULL]

#   return(df)
# }
