#' S7 class for hespeR vectors
#'
#' @typed hesper_var: character[1]
#'  A single HESPER item name.
#' @typed hesper_vals: character[1+]
#'  A vector of allowed options for the HESPER item.
#' @typedreturn S7_object
#'  A S7 `HesperVector` object representing a HESPER item and its allowed options.
#'
#' @details
#' The `HesperVector` class is designed to encapsulate the core properties of HESPER data. It includes the name of a HESPER item (`hesper_var`) and a vector of allowed options for that item (`hesper_vals`).
#' The class validates that the `hesper_var` property is a valid Hesper item (see [hespeR::hesper_vars]) and that all values in `hesper_vals` are within the allowed options (see specified in [hespeR::hesper_opts]).
#'
#' @export
HesperVector <- S7::new_class(
  "HesperDefault",
  properties = list(
    hesper_var = S7::class_character,
    hesper_vals = S7::class_character
  ),
  validator = function(self) {
    #  hesper_var is a character scalar matching a HESPER item
    checkmate::assert_character(self@hesper_var, len = 1)
    allowed_vars <- hesper_vars
    check_values_in_set(
      self@hesper_var,
      allowed_vars,
      property = 'hesper_var'
    )

    # hesper_vals is a character vector with at least one value of HESPER options
    checkmate::assert_character(self@hesper_vals, min.len = 1)
    allowed_opts <- hesper_opts
    check_values_in_set(
      self@hesper_vals,
      allowed_opts,
      property = 'hesper_vals'
    )

    NULL
  }
)
