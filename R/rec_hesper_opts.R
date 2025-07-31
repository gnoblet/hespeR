#' Recode response options to allowed values for HESPER items
#'
#' This function recodes response options in HESPER items to a standardized set of allowed values.
#'
#' @typed df:
#'   A data frame containing HESPER items.
#' @typed hesper_vars:
#'   A character vector of HESPER item column names to recode.
#' @typed old_hesper_opts:
#'   A named character vector of response options mapped to allowed response options for HESPER items. Names are allowed options and values are the old options to recode from.
#'
#' @typedreturn
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
  checkmate::assert_subset(
    names(old_hesper_opts),
    hespeR::hesper_opts
  )

  # df is a data frame, hesper_vars are in df and all these vars are made of old_hesper_opts
  check_vars_in_set(df, hesper_vars, old_hesper_opts)

  #------ Recode

  df[hesper_vars] <- lapply(df[hesper_vars], function(x) {
    forcats::fct_recode(x, !!!old_hesper_opts)
  })

  return(df)
}
