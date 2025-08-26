#' S7 class for a single HESPER category
#'
#' @typed cat: character[1]
#'   A single category name (e.g., "fsl", "health", "protection").
#' @typed vars: character[1+]
#'   A vector of HESPER variable names that belong to this category.
#'
#' @typedreturn S7_object
#'   A S7 object of class `HesperCategory`, representing a single category and its associated HESPER variables.
#'
#' @details
#' The `HesperCategory` class represents a single thematic category of HESPER items (e.g., health, protection, WASH).
#' It validates that the category name is a non-empty string and that all HESPER variables are valid according to `hesper_vars()`.
#'
#' @export
HesperCategory <- S7::new_class(
  "HesperCategory",
  properties = list(
    cat = S7::class_character,
    vars = S7::class_character
  ),
  validator = function(self) {
    # cat is a character scalar
    checkmate::assert_character(
      self@cat,
      len = 1,
      min.chars = 1,
      any.missing = FALSE,
      null.ok = FALSE
    )

    # vars is a character vector with at least one value
    checkmate::assert_character(
      self@vars,
      min.len = 1,
      min.chars = 1,
      any.missing = FALSE,
      null.ok = FALSE
    )

    # there are no duplicates in vars
    check_dupes(self@vars, glue::glue("vars for category '{self@cat}'"))

    # all vars are valid HESPER variables
    allowed_vars <- hesper_vars()
    check_values_in_set(
      self@vars,
      allowed_vars,
      property = 'vars'
    )

    # no duplicate vars
    check_dupes(self@vars, glue::glue("vars for category '{self@cat}'"))

    NULL
  }
)

#' S7 class for HESPER categories list
#'
#' @typed category_list: list[HesperCategory]
#'   A list of `HesperCategory` items, each representing a thematic category and its associated HESPER variables.
#'
#' @typedreturn S7_object
#'   A S7 object of class `HesperCategories`, representing a collection of HESPER categories.
#'
#' @details
#' The `HesperCategories` class is designed to encapsulate a collection of thematic categories for HESPER items.
#' Each category in the list is an instance of the `HesperCategory` class.
#'
#' The class validates that:
#' - Each item in the list is a valid `HesperCategory`
#' - Category names (cat) are unique across all categories
#' - HESPER variables (vars) are not duplicated across categories (each variable belongs to only one category)
#' - The list is not empty
#'
#' @export
HesperCategories <- S7::new_class(
  "HesperCategories",
  properties = list(
    category_list = S7::class_list
  ),
  validator = function(self) {
    # category_list is a non-empty list
    checkmate::assert_vector(self@category_list, min.len = 1)

    # all items are HesperCategory instances and validate them
    check_vector_class(
      self@category_list,
      HesperCategory,
      "category_list",
      use_S7_inherits = TRUE
    )
    purrr::map(self@category_list, \(x) HesperCategory@validator(x))

    # category names (cat) are unique
    category_names <- purrr::map_chr(self@category_list, \(x) x@cat)
    check_dupes(category_names, "category names (cat) in 'category_list'")

    # HESPER variables (vars) are not duplicated across categories
    all_hesper_vars <- purrr::map(self@category_list, \(x) x@vars) |>
      purrr::list_c()
    check_dupes(all_hesper_vars, "HESPER variables (vars) across categories")

    NULL
  }
)
