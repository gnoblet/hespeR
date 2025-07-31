#' S7 class for hespeR data.frames (default)
#'
#' @typed df: data.frame[,1+]
#'  A data frame containing HESPER items with HESPER response options.
#'
#' @details
#' The `HesperDefault` class is designed to encapsulate the core properties of HESPER data. It includes a data frame containing HESPER items and a character vector of allowed options for those items.
#' The class validates that the `df` property is a data frame with columns that match the allowed HESPER variables (see [hespeR::hesper_vars]) and all values in the data frame are within the allowed options (see specified in [hespeR::hesper_opts]).
#'
#' @export
HesperDefault <- S7::new_class(
  "HesperDefault",
  properties = list(df = S7::new_property(S7::class_data.frame)),
  validator = function(self) {
    # Validate data as follows:
    # 1. Check that the data is a data frame with at least one column.
    # 2. Check that all columns are in the allowed HESPER variables. Throws a warning if allowed columns are missing.
    # 3. Check that all values in the data frame are within the allowed HESPER options.

    # 1.
    checkmate::assert_data_frame(self@df, min.cols = 1)

    # 2.
    allowed_vars <- hesper_vars
    check_values_in_set(
      colnames(self@df),
      allowed_vars,
      property = 'df columns'
    )

    # 3.
    allowed_opts <- hesper_opts
    check_vars_in_set(self@df, colnames(self@df), allowed_opts)

    # WARN if validator passes
    vars_nin <- setdiff(allowed_vars, colnames(self@df))
    if (length(vars_nin) > 0) {
      rlang::warn(
        msg_missing_vars('df', vars_nin, property = 'df columns', i = FALSE)
      )
    }

    NULL
  }
)
