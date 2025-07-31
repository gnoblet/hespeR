#' S7 class for skip logic (SL)
#'
#' @field hesper_var Name of the HESPER variable (character).
#' @field subset_var Name of the subset variable (character).
#' @field subset_vals Values in the subset variable (character).
#' @export
SL <- S7::new_class(
  "SL",
  properties = list(
    hesper_var = S7::class_character,
    subset_var = S7::class_character,
    subset_vals = S7::class_character
  )
)


#' S7 class for hespeR data.frames (default)
#'
#' @field data The main data as a data.frame.
#' @field hesper_opts Allowed levels for HESPER items (character).
#'
#' @details
#' The `HesperDefault` class is designed to encapsulate the core properties of HESPER data. It includes a data frame containing HESPER items and a character vector of allowed options for those items.
#' The class validates that:
#' - The `hesper_opts` property contains exactly 5 allowed options.
#' - The `data` property is a data frame with columns that match the allowed HESPER variables.
#' - All values in the data frame columns are within the allowed options specified in [hespeR::hesper_opts].
#'
#' @export
HesperDefault <- S7::new_class(
  "HesperDefault",
  properties = list(
    data = S7::new_property(
      S7::class_data.frame
    ),
    hesper_opts = S7::new_property(
      S7::class_character
    )
  ),
  validator = function(self) {
    # Validate hesper_opts
    checkmate::assert_character(
      self@hesper_opts,
      min.chars = 1,
      len = 5,
      any.missing = FALSE,
      null.ok = FALSE
    )

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


#' S7 class for hespeR data.frames (enhanced)
#'
#' Inherits from HesperDefault. May have additional properties or methods.
#' @export
HesperEnhanced <- S7::new_class(
  "HesperEnhanced",
  parent = HesperDefault,
  properties = list(),
  validator = function(self) {
    hesper_cols <- intersect(names(self@data), hesper_vars)
    invalid <- lapply(self@data[hesper_cols], function(col) {
      bad <- setdiff(unique(na.omit(col)), self@hesper_opts)
      if (length(bad) > 0) bad else NULL
    })
    invalid <- Filter(Negate(is.null), invalid)
    if (length(invalid) > 0) {
      return(paste0(
        "Invalid values in columns: ",
        paste(names(invalid), collapse = ", "),
        ". Only allowed: ",
        paste(self@hesper_opts, collapse = ", ")
      ))
    }
    NULL
  }
)
