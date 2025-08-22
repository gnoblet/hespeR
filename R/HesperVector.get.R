#' Get property from HesperVector
#'
#' Generic accessor for HesperVector properties
#'
#' @typed self: HesperVector S7 object
#'   A HesperVector object
#' @typed property: character[1]
#'  Name of the property to access. See `HesperVector@properties` for allowed properties.
#' @typedreturn The value of the requested property
#'
#' @export
HesperVector.get <- function(self, property) {
  #------ Checks

  # HesperVector is a HesperVector
  checkmate::assertClass(self, "hespeR::HesperVector")
  HesperVector@validator(self)

  # property is a single character string in the allowed set
  checkmate::assert_string(property, na.ok = FALSE)
  checkmate::assert_subset(
    property,
    HesperVector@properties |> names()
  )

  #------ Get
  self@property
}

#' @rdname HesperVector.get
HesperVector.get_hesper_var <- function(hesper_vector) {
  HesperVector.get(hesper_vector, "hesper_var")
}

#' @rdname HesperVector.get
HesperVector.get_hesper_vals <- function(hesper_vector) {
  HesperVector.get(hesper_vector, "hesper_vals")
}

#' @rdname HesperVector.get
HesperVector.get_allow_missing <- function(hesper_vector) {
  HesperVector.get(hesper_vector, "allow_missing")
}

#' @rdname HesperVector.get
HesperVector.get_hesper_bins <- function(object) {
  HesperVector.get(object, "hesper_bins")
}
