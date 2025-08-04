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
