#' S7 class for hespeR list of vectors
#'
#' @typed hesper_list: list[HesperVector]
#'   A list of `HesperList` items, each representing a HESPER item and its allowed options.
#' @typedreturn S7_object
#'  A S7 object of class `HesperList`, representing a list of HESPER vectors.
#'
#' @details
#' The `HesperList` class is designed to encapsulate a collection of HESPER vectors. Each vector in the list is an instance of the `HesperVector` class, representing a specific HESPER item and its allowed options.
#'
#' The class validates that each item in the list is a valid `HesperVector` and that the list is not empty nor has duplicated HESPER items.
#'
#' @export
HesperList <- S7::new_class(
  "HesperList",
  properties = list(
    hesper_list = S7::class_vector
  ),
  validator = function(self) {
    # hesper_list is a non-empty list
    checkmate::assert_vector(self@hesper_list, min.len = 1)

    # all items are HesperVector instances
    check_vector_class(self@hesper_list, "hespeR::HesperVector", 'hesper_list')

    # duplicated hesper_vars
    hesper_vars <- purrr::map_chr(self@hesper_list, \(x) x@hesper_var)
    check_dupes(hesper_vars, "@hesper_var properties of 'hesper_list'")

    NULL
  }
)
