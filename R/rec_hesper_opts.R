#' Recode response options to allowed values for HESPER items
#'
#' This function recodes response options in HESPER items to a standardized set of allowed values (see [hespeR::hesper_opts] for the allowed response options).
#'
#' @typed df: data.frame[,1+]
#'   A data frame containing HESPER items.
#' @typed hesper_vars: character[1+]
#'   Names of HESPER item column names to recode.
#' @typed old_hesper_opts: character_named[5]
#'   Response options mapped to allowed response options for HESPER items. Names are allowed options and values are the old options to recode from.
#'
#' @typedreturn data.frame[,1+]
#'   A data frame with HESPER items recoded to the allowed response options.
#'
#' @export
rec_hesper_opts <- function(df, hesper_vars, old_hesper_opts) {
  #------ Checks

  # old_hesper_opts is a named character vector of length 5 where names are new options and values are old options in hesper_opts
  checkmate::assert_character(
    old_hesper_opts,
    any.missing = FALSE,
    len = 5,
    names = "named"
  )
  check_values_in_set(
    old_hesper_opts,
    hesper_opts,
    property = "old_hesper_opts"
  )

  # df is a data frame, hesper_vars are in df and all these vars are made of old_hesper_opts
  check_vars_in_set(df, hesper_vars, old_hesper_opts)

  #------ Recode

  df[hesper_vars] <- lapply(df[hesper_vars], function(x) {
    forcats::fct_recode(x, !!!old_hesper_opts)
  })

  return(df)
}
