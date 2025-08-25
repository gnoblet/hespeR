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
#' The class validates that each item in the list is a valid `HesperVector` and that the list is not empty, has duplicated HESPER items, has different lengths of `hesper_vals`, and has consistent `allow_missing` values across all items.
#'
#' @export
HesperList <- S7::new_class(
  "HesperList",
  properties = list(
    hesper_list = S7::class_list
  ),
  validator = function(self) {
    # hesper_list is a non-empty list
    checkmate::assert_vector(self@hesper_list, min.len = 1)

    # all items are HesperVector instances and validate them using HesperVector@validator
    check_vector_class(
      self@hesper_list,
      HesperVector,
      "hesper_list",
      use_S7_inherits = TRUE
    )
    purrr::map(self@hesper_list, \(x) HesperVector@validator(x))

    # duplicated hesper_vars
    hesper_vars <- purrr::map_chr(self@hesper_list, \(x) x@hesper_var)
    check_dupes(hesper_vars, "@hespr_var properties of 'hesper_list'")

    # all items @hesper_opts are of same length
    hesper_opts_lengths <- purrr::map_int(self@hesper_list, \(x) {
      length(x@hesper_vals)
    })
    if (length(unique(hesper_opts_lengths)) > 1) {
      rlang::abort(c(
        glue::glue(
          "Not all items in 'hesper_list' have the same length for @hesper_vals."
        )
      ))
    }

    # # all items have the same @allow_missing value
    # allow_missing_values <- purrr::map_lgl(self@hesper_list, \(x) {
    #   x@allow_missing
    # })
    # if (length(unique(allow_missing_values)) > 1) {
    #   rlang::abort(c(
    #     glue::glue(
    #       "Not all items in 'hesper_list' have the same value of @allow_missing."
    #     )
    #   ))
    # }

    NULL
  }
)
