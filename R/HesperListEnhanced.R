#' S7 class for hespeR list of vectors with skip logic
#'
#' @typed hesper_list: list[HesperVector]
#'   A list of `HesperList` items, each representing a HESPER item and its allowed options.
#' @typed SL: list[hespeR::SL]
#'   `SL` objects representing skip logic rules.
#' @typed other_list: list[vec]
#'   Other vectors associated with the HESPER list, typically used for skip logic rules or disaggregation.
#' @typedreturn S7_object
#'   A S7 object of class `HesperListEnhanced`, representing a list of HESPER vectors associated with skip logic rules and additional data.
#' 
#' @export
HesperListEnhanced <- S7::new_class(
  "HesperListEnhanced",
  parent = HesperList,
  properties = list(
    hesper_list = S7::class_list,
    SL = S7::class_list,
    other_list = S7::class_list
  ),
  validator = function(self) {
    HesperList@validator(self)
    if (length(self@SL) > 0) {
      check_vector_class(self@SL, "hespeR::SL", "SL")
      purrr::map(self@SL, function(x) {
        SL@validator(x)
      })
      sl_hesper_vars <- purrr::map_chr(self@SL, \(x) x@hesper_var)
      hl_hesper_vars <- purrr::map_chr(self@hesper_list, \(x) x@hesper_var)
      missing_hesper_vars <- setdiff(sl_hesper_vars, hl_hesper_vars)
      if (length(missing_hesper_vars) > 0) {
        rlang::abort(c(
          "Hesper variables in the SL list must be present in hesper_list. ",
          "i" = glue::glue(
            "The following hesper_vars from SL are missing in hesper_list: ",
            glue::glue_collapse(
              missing_hesper_vars,
              sep = ', ',
              last = ' and '
            )
          )
        ))
      }
      sl_subset_vars <- purrr::map_chr(self@SL, \(x) x@subset_var)
      other_list_names <- names(self@other_list)
      missing_subset_vars <- setdiff(sl_subset_vars, other_list_names)
      if (length(missing_subset_vars) > 0) {
        rlang::abort(c(
          "Subset variables in the SL list must be present in other_list. ",
          "i" = glue::glue(
            "The following subset_vars from SL are missing in other_list: ",
            glue::glue_collapse(missing_subset_vars, sep = ", ", last = " and ")
          )
        ))
      }
    }
    if (length(self@other_list) > 0) {
      len_hl <- purrr::map_int(self@hesper_list, \(x) {
        length(x@hesper_vals)
      }) |>
        unique()
      other_list_lengths <- purrr::map_int(self@other_list, \(x) {
        length(x)
      })
      if (
        length(unique(other_list_lengths)) > 1 ||
          !all(other_list_lengths == len_hl)
      ) {
        rlang::abort(c(
          glue::glue(
            "All items in 'other_list' must have the same length as items in 'hesper_list'."
          )
        ))
      }
    }
    NULL
  }
)
