#' S7 class for HESPER skip logic (SL)
#'
#' @typed hesper_var: character[1]
#'  Name of the HESPER variable.
#' @typed subset_var: character[1]
#'  Name of the subset variable.
#' @typed subset_vals: character[1+]
#'  Values in the subset variable that are skipped.
#'
#' @details
#' The `SL` class is designed to encapsulate the properties of a skip logic rule in HESPER. It includes the name of the HESPER variable, the name of the subset variable, and the values in the subset variable.
#' The class validates that:
#' - The `hesper_var` property is a valid HESPER variable.
#' - The `subset_var` property is a character string.
#' - The `subset_vals` property is a vector with at least one value.
#'
#' @export
SL <- S7::new_class(
  "SL",
  properties = list(
    hesper_var = S7::class_character,
    subset_var = S7::class_character,
    subset_vals = S7::class_vector
  ),
  validator = function(self) {
    # Validate that hesper_var is a valid HESPER variable
    checkmate::assert_character(self@hesper_var, len = 1)
    check_values_in_set(
      self@hesper_var,
      hesper_vars,
      property = 'hesper_var'
    )

    # Validate that subset_var is a valid variable name
    checkmate::assert_character(self@subset_var, len = 1)

    # Validate that subset_vals contains valid values for the subset variable
    checkmate::assert_vector(self@subset_vals, min.len = 1)

    NULL
  }
)
