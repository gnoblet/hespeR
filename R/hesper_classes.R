#' S7 class for skip logic (SL)
#'
#' @field hesper_var Name of the HESPER variable (character).
#' @field subset_var Name of the subset variable (character).
#' @field subset_vals Values in the subset variable (character).
#'
#' @details
#' The `SL` class is designed to encapsulate the properties of a skip logic rule in HESPER. It includes the name of the HESPER variable, the name of the subset variable, and the values in the subset variable.
#' The class validates that:
#' - The `hesper_var` property is a valid HESPER variable.
#' - The `subset_var` property is a valid variable name.
#' - The `subset_vals` property contains valid values for the subset variable.
#'
#' @export
SL <- S7::new_class(
  "SL",
  properties = list(
    hesper_var = S7::class_character,
    subset_var = S7::class_character,
    subset_vals = S7::class_character
  )
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
