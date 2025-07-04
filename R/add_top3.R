## unite the thre priority columns to have one select multiple hesper priorities

#' Add a new column with the top three priorities
#' @param df input dataframe
#' @param new_var name of the new column
#' @param vars_unite vector with column names corresponding to top 1/2/3 priority to combine in unordered top three priorities
#'
#' @return dataframe with new column
#' @export
#'
add_top3 <- function(df, new_var, vars_unite) {
  #------ Checks

  # df is a dataframe
  checkmate::assertDataFrame(df)

  # df is not data.table, convert it
  if (!checkmate::testDataTable(df)) {
    rlang::warn("Converting df to data.table.")
    data.table::setDT(df)
  }

  # new_var is character of length 1
  checkmate::assertCharacter(new_var, len = 1, min.chars = 1)

  # vars_unite is a character vector
  checkmate::assertCharacter(vars_unite, min.chars = 1)

  # all vars are in df and of class character
  for (var in vars_unite) {
    if (!(var %in% colnames(df))) {
      rlang::abort(paste0("Variable ", var, " not found in df."))
    }
    checkmate::assertClass(df[[var]], "character", .var.name = var)
  }

  #------ Do stuff
  df[,
    (new_var) := {
      temp <- do.call(
        paste,
        c(lapply(.SD, function(x) fifelse(is.na(x), "", x)), sep = " ")
      )
      fifelse(
        stringr::str_trim(temp) == "",
        NA_character_,
        stringr::str_trim(temp)
      )
    },
    .SDcols = vars_unite
  ]

  return(df)
}
