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
    # 2. Check that there is at least one HESPER column in the data frame.
    # 3. Check that all values in the HESPER columns are within the allowed HESPER options.
    # Extra columns are allowed and ignored by validation.

    # 1.
    checkmate::assert_data_frame(self@df, min.cols = 1)

    # Identify HESPER columns present in df
    allowed_vars <- hesper_vars
    df_cols <- colnames(self@df)
    hesper_cols <- intersect(df_cols, allowed_vars)

    # 2.
    if (length(hesper_cols) == 0) {
      rlang::abort(
        'No HESPER columns found in df. The data frame must contain at least one HESPER column.'
      )
    }

    # 3.
    allowed_opts <- hesper_opts
    check_vars_in_set(
      self@df[, hesper_cols, drop = FALSE],
      hesper_cols,
      allowed_opts
    )

    # WARN if validator passes
    vars_nin <- setdiff(allowed_vars, hesper_cols)
    if (length(vars_nin) > 0) {
      rlang::warn(
        msg_missing_vars('df', vars_nin, property = 'df columns', i = FALSE)
      )
    }

    NULL
  }
)
