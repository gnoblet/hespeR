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
as_data_frame <- S7::new_generic("as_data_frame", dispatch_args = "x")

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

# #' Convert data.frame to HesperList
# #'
# #' @typed df: dat
# #'  A data.frame with one column per HESPER variable, each column a character vector of options.
# #' @typed allow_missing: logical[1:ncol(df)]
# #'  Optional logical vector, or will use attr(df, "allow_missing") if present.
# #'
# #' @return A HesperList object.
# #' @export
# from_data_frame <- function(df, allow_missing = NULL) {
#   # if allow missing is not an attribute of df, check allow_missing is provided

#   if (!'allow_missing' %in% names(attributes(df))) {
#     if (is.null(allow_missing)) {
#       rlang::abort(
#         "allow_missing must be provided or present as an attribute on df."
#       )
#     }
#   }
#   if (!is.logical(allow_missing)) {
#     rlang::abort("allow_missing must be a logical vector.")
#   }
#   if (length(allow_missing) != 1) {
#     rlang::abort(
#       paste0(
#         "Length of allow_missing must be 1"
#       )
#     )
#   }
#   hesper_list <- purrr::map2(
#     names(df),
#     seq_along(df),
#     \(var, i) {
#       HesperVector(
#         hesper_var = var,
#         hesper_vals = as.character(df[[i]]),
#         allow_missing = allow_missing
#       )
#     }
#   )
#   HesperList(hesper_list = hesper_list)
# }
