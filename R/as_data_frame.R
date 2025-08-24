#' Convert HesperList to data.frame
#'
#' Returns a data.frame representation of a HesperVector, a HesperList, or a HesperListEnhanced.
#'
#' @typed x: HesperVector | HesperList | HesperListEnhanced
#' An S7 object.
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

#' @rdname as_data_frame
#' @name as_data_frame
#'
S7::method(as_data_frame, HesperVector) <- function(x, bins = TRUE) {
  df <- data.frame(
    purrr::set_names(list(x@hesper_vals), x@hesper_var),
    stringsAsFactors = FALSE
  )

  if (bins) {
    hesper_bins <- x@hesper_bins |> as.data.frame()
    colnames(hesper_bins) <- glue::glue(
      x@hesper_var,
      ".{colnames(hesper_bins)}"
    )
    df <- cbind(df, hesper_bins)
  }
  attr(df, "allow_missing") <- x@allow_missing
  df
}

#' @rdname as_data_frame
#' @name as_data_frame
#'
S7::method(as_data_frame, HesperList) <- function(x, bins = TRUE) {
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


#' @rdname as_data_frame
#' @name as_data_frame
#'
#' @typed x: HesperListEnhanced
#' A HesperListEnhanced object.
#'
S7::method(as_data_frame, HesperListEnhanced) <- function(x, bins = TRUE) {
  # Convert hesper_list to data.frame using HesperList method
  df <- as_data_frame(
    HesperList(
      hesper_list = x@hesper_list
    ),
    bins = bins
  )

  # Add other_list columns if present
  if (length(x@other_list) > 0) {
    other_df <- as.data.frame(x@other_list, stringsAsFactors = FALSE)
    df <- cbind(df, other_df)
  }
  df
}
