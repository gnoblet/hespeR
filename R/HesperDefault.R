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
  properties = list(data = S7::new_property(S7::class_data.frame)),
  validator = function(self) {
    # hesper_opts are the allowed options for HESPER items
    allowed_opts <- hesper_opts
    check_values_in_set(
      self@hesper_opts,
      allowed_opts,
      property = "hesper_opts"
    )

    if (!all(self@hesper_opts %in% allowed_opts)) {
      rlang::abort(
        c(
          'Invalid values in hesper_opts.',
          '*' = glue::glue(
            'Following values are not allowed: ',
            glue::glue_collapse(
              setdiff(self@hesper_opts, allowed_opts),
              sep = ', ',
              last = ', and '
            )
          ),
          'i' = glue::glue(
            'Values must be one of: ',
            glue::glue_collapse(allowed_opts, sep = ', ', last = ', and ')
          )
        )
      )
    }

    # Validate data
    allowed_vars <- hesper_vars
    checkmate::assert_data_frame(self@data)
    checkmate::assert_subset(colnames(self@data), allowed_vars)
    vars_bad_levels <- c()
    for (var in names(self@data)) {
      bad <- setdiff(unique(na.omit(self@data[[var]])), self@hesper_opts)
      if (length(bad) > 0) {
        vars_bad_levels <- c(vars_bad_levels, var)
      }
    }
    if (length(vars_bad_levels) > 0) {
      rlang::abort(
        c(
          'Invalid values in vars.',
          '*' = glue::glue(
            'Following vars contain wrong values:',
            glue::glue_collapse(
              vars_bad_levels,
              sep = ',',
              last = ', and'
            )
          ),
          'i' = glue::glue(
            'Values must be one of: ',
            glue::glue_collapse(
              self@hesper_opts,
              sep = ',',
              last = ', and '
            )
          )
        )
      )
    }
    NULL
  }
)
