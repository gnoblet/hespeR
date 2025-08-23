#' Convert HesperList to data.frame
#'
#' Returns a data.frame representation of a HesperList object.
#'
#' @typed x: HesperList
#' A HesperList object.
#' @typed bins: logical[1]
#'  If `TRUE`, includes binary columns for each HESPER option in the data.frame.
#'
#' @typedreturn data.frame
#'  With columns named by `hesper_var` and values from `hesper_vals` (and optionally binaries). An attribute `allow_missing` is added to indicate if missing values are allowed for each variable.
#'
#' @export
as_data_frame <- S7::new_generic(
  "as_data_frame",
  dispatch_args = "x",
  function(x, bins = TRUE) {
    S7::S7_dispatch()
  }
)

S7::method(as_data_frame, HesperList) <- function(x, bins = TRUE) {
  # Each HesperVector becomes a column, named by hesper_var, values are hesper_vals, allow_missing is a logical vector which goes as an attribute
  hesper_vars <- purrr::map_chr(x@hesper_list, \(x) x@hesper_var)
  hesper_vals_list <- purrr::map(x@hesper_list, \(x) x@hesper_vals)
  allow_missing <- unique(purrr::map_lgl(x@hesper_list, \(x) x@allow_missing))

  df <- as.data.frame(
    purrr::set_names(hesper_vals_list, hesper_vars),
    stringsAsFactors = FALSE
  )

  if (bins) {
    hesper_bins <- purrr::map(x@hesper_list, function(x) {
      var_name <- x@hesper_var
      hesper_bins <- x@hesper_bins
      df <- x@hesper_bins |> as.data.frame()
      colnames(df) <- glue::glue(var_name, ".{colnames(df)}")
      df
    }) |>
      purrr::list_cbind()

    df <- cbind(df, hesper_bins)
  }
  attr(df, "allow_missing") <- allow_missing
  df
}
