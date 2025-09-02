#' S7 class for hespeR list of vectors with skip logic
#'
#' @typed hesper_list: list[HesperVector]
#'   A list of `HesperList` items, each representing a HESPER item and its allowed options.
#' @typed SL: list[SL]
#'   `SL` objects representing skip logic rules.
#' @typed other_list: list[vec]
#'   Other vectors associated with the HESPER list, typically used for skip logic rules or disaggregation.
#' @typed priority_list: list[HesperPriorities]
#'   `HesperPriorities` list of length 1 representing top 3 priorities for respondents.  When provided, the validator will ensure that the priorities correspond to items that were indicated as serious problems in the HESPER list.
#' @typed category_list: list[HesperCategories]
#'   `HesperCategories` list of length 1 representing categorization of HESPER items into meaningful groups (e.g., sectors). When provided, allows for categorical analysis of HESPER data.
#'
#' @typedreturn S7_object
#'   A S7 object of class `HesperListEnhanced`, representing a list of HESPER vectors associated with skip logic rules, priorities, categories, and additional data.
#'
#' @section Getters:
#'
#' `category_list_pp` is a getter property that returns a list of category-level aggregated HESPER vectors when `category_list` is provided. Each category becomes a named character vector (with associated binaries) with aggregated responses following hierarchical logic:
#' - "serious_problem" if any variable in the category is "serious_problem"
#' - "pnta" if any variable in the category is "pnta" (and none are "serious_problem")
#' - "dnk" if any variable in the category is "dnk" (and none are "serious_problem" or "pnta")
#' - "no_serious_problem" if ALL variables in the category are "no_serious_problem"
#' - "not_applicable" if ALL variables in the category are "not_applicable"
#' If `category_list` is not provided, it returns an empty list.
#'
#' **Automatic Skip Logic Application:**
#' Both `hesper_list` and `priority_list` properties automatically apply skip logic rules when `SL` is provided:
#' - `hesper_list_pp`: Returns cleaned HESPER data with skip logic applied. If no skip logic rules are defined (SL list is empty), returns the original hesper_list unchanged.
#' - `priority_list_pp`: Returns cleaned priority data with skip logic applied using "missing" cascading (if higher priority is NA, cascade NA down). If no skip logic rules are defined or no priority_list is provided, returns the original priority_list unchanged.
#'
#' This provides transparent access to cleaned data without needing to manually call skip logic application methods.
#'
#' @details
#'  This class extends the `HesperList` class to include support for skip logic (SL) rules, respondent priorities, categorization capabilities, and additional data. Note that the only non empty list that must be provided is `hesper_list`. The other lists can be empty (length 0).
#'
#' It validates the following:
#'
#' - Each item in `SL` must be a valid `hespeR::SL` object.
#' - The `hesper_var` in each `SL` object must correspond to a `hesper_var` in `hesper_list`.
#' - The `subset_var` in each `SL` object must correspond to a name in `other_list`.
#' - All items in `other_list` must have the same length as items in `hesper_list`.
#' - If `priority_list` is provided, it must be a valid `HesperPriorities` object and its vectors must have the same length as items in `hesper_list`.
#' - Each priority in `priority_list` must correspond to a `hesper_var` in `hesper_list` that was marked as "serious_problem" at the same position.
#' - If `category_list` is provided, it must be a valid `HesperCategories` object and all HESPER variables in its categories must be present in `hesper_list`.
#'
#' @export
HesperListEnhanced <- S7::new_class(
  "HesperListEnhanced",
  parent = HesperList,
  properties = list(
    # inputs
    hesper_list = S7::class_list,
    SL = S7::class_list,
    other_list = S7::class_list,
    priority_list = S7::class_list,
    category_list = S7::class_list,
    # preprocessed / pp
    hesper_list_pp = S7::new_property(
      S7::class_list,
      getter = function(self) {
        apply_hle_hesper_list_sl(self)
      }
    ),
    priority_list_pp = S7::new_property(
      S7::class_list,
      getter = function(self) {
        apply_hle_priority_list_sl(self)
      }
    ),
    category_list_pp = S7::new_property(
      S7::class_list,
      getter = function(self) {
        apply_hle_hesper_list_cat(self)
      }
    )
  ),
  validator = function(self) {
    HesperList@validator(self)

    # get all hesper_vars in hesper_list
    hl_hesper_vars <- purrr::map_chr(self@hesper_list, \(x) x@hesper_var)

    # Validate SL if provided
    validate_hle_sl(self@SL, hl_hesper_vars, self@other_list)

    # Validate other_list if provided
    validate_hle_other_list(self@other_list, self@hesper_list)

    # Validate priority_list if provided
    validate_hle_priority_list(
      self@priority_list,
      self@hesper_list,
      hl_hesper_vars
    )

    # Validate category_list if provided
    validate_hle_category_list(self@category_list, hl_hesper_vars)

    NULL
  }
)
