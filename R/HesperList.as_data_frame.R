hespeR / R / methodsHesperList.R
#' Convert HesperList to data.frame
#'
#' Returns a data.frame representation of a HesperList object.
#'
#' @param x A HesperList object.
#' @return A data.frame with columns named by `hesper_var` and values from `hesper_vals`. An attribute `allow_missing` is added to indicate if missing values are allowed for each variable.
#' @export
as_data_frame <- S7::new_generic("as_data_frame", dispatch_args = "x")

S7::method(as_data_frame, HesperList) <- function(x) {
  # Each HesperVector becomes a column, named by hesper_var, values are hesper_vals, allow_missing is a logical vector which goes as an attribute
  hesper_vars <- purrr::map_chr(x@hesper_list, \(x) x@hesper_var)
  hesper_vals_list <- purrr::map(x@hesper_list, \(x) x@hesper_vals)
  allow_missing_vec <- purrr::map_lgl(x@hesper_list, \(x) x@allow_missing)
  df <- as.data.frame(
    purrr::set_names(hesper_vals_list, hesper_vars),
    stringsAsFactors = FALSE
  )
  attr(df, "allow_missing") <- allow_missing_vec
  df
}

#' Convert data.frame to HesperList
#'
#' @typed df: dat
#' A data.frame with one column per HESPER variable, each column a character vector of options.
#' @param allow_missing Optional logical vector, or will use attr(df, "allow_missing") if present.
#' @return A HesperList object.
#' @export
from_data_frame <- function(df, allow_missing = NULL) {
  if (is.null(allow_missing)) {
    allow_missing <- attr(df, "allow_missing")
    if (is.null(allow_missing)) {
      rlang::abort(
        "allow_missing must be provided or present as an attribute on df."
      )
    }
  }
  if (!is.logical(allow_missing)) {
    rlang::abort("allow_missing must be a logical vector.")
  }
  if (length(allow_missing) != ncol(df)) {
    rlang::abort(
      paste0(
        "Length of allow_missing (",
        length(allow_missing),
        ") does not match number of columns in df (",
        ncol(df),
        ")."
      )
    )
  }
  hesper_list <- purrr::map2(
    names(df),
    seq_along(df),
    \(var, i) {
      HesperVector(
        hesper_var = var,
        hesper_vals = as.character(df[[i]]),
        allow_missing = allow_missing[[i]]
      )
    }
  )
  HesperList(hesper_list = hesper_list)
}
