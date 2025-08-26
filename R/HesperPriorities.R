#' S7 class for HESPER priorities
#'
#' @typed top1: character[1+]
#'  Top 1 priority
#' @typed top2: character[1+]
#'  Top 2 priority
#' @typed top3: character[1+]
#'  Top3 priority
#' @typed allow_missing: logical[1]
#'  Whether to allow missing values (NA) in the priority vectors. Default is FALSE.
#'
#' @typedreturn S7_object
#'  A S7 'HesperPriorities' object representing HESPER priorities.
#'
#' @details
#'  This class is used to represent and validate HESPER priorities data. At the end of the HESPER module, respondents are asked to rank their top three priorities from the list of items for which they responded they had a serious problem. This class ensures that the provided priorities are valid according to the following rules:
#' - Each of `top1`, `top2`, and `top3` must be a character vector of the same length, representing the priorities for each respondent.
#' - Each priority must correspond to a valid HESPER item. The function `hesper_vars()` provides the list of valid HESPER items.
#' - Each priority must be unique within each respondent. That is, no respondent can have the same item ranked in multiple priority positions.
#' - The `allow_missing` property determines whether missing values (NA) are allowed in the priority vectors. If `TRUE`, NA values are permitted; if `FALSE`, all priorities must be valid HESPER items.
#'
#' When a proprerty of [HesperListEnhanced], the validator will also ensure that the priorities correspond to items that were indicated as serious problems in the HESPER list.
#'
#'
#' @export
HesperPriorities <- S7::new_class(
  "HesperPriorities",
  properties = list(
    top1 = S7::class_character,
    top2 = S7::class_character,
    top3 = S7::class_character,
    allow_missing = S7::new_property(
      S7::class_logical,
      default = FALSE
    )
  ),
  validator = function(self) {
    # each is a character vector of
    checkmate::assert_character(self@top1, min.len = 1)
    checkmate::assert_character(self@top2, min.len = 1)
    checkmate::assert_character(self@top3, min.len = 1)

    # allow_missing is a logical scalar

    # all have same length
    top_lengths <- c(
      length(self@top1),
      length(self@top2),
      length(self@top3)
    )
    if (length(unique(top_lengths)) > 1) {
      rlang::abort(c(
        glue::glue(
          "Not all 'top*' have the same length."
        )
      ))
    }

    # all must be match a HESPER item
    allowed_vars <- if (self@allow_missing) {
      c(hesper_vars(), NA)
    } else {
      hesper_vars()
    }
    check_values_in_set(
      self@top1,
      allowed_vars,
      property = "top1"
    )
    check_values_in_set(
      self@top2,
      allowed_vars,
      property = "top2"
    )
    check_values_in_set(
      self@top3,
      allowed_vars,
      property = "top3"
    )

    # must be unique item-wise
    are_all_diff <- purrr::pmap_lgl(
      list(self@top1, self@top2, self@top3),
      \(x, y, z) {
        vals <- c(x, y, z)
        # check if x, y, z are all different OR NA
        # just need to check uniqueness of non-NA values
        vals <- vals[!is.na(vals)]
        length(unique(vals)) == length(vals)
      }
    )
    if (!all(are_all_diff)) {
      rlang::abort(c(
        "Some item positions have non-unique hesper priorities.",
        "i" = glue::glue(
          "The following positions have non-unique priorities:",
          glue::glue_collapse(which(!are_all_diff), sep = ", ")
        ),
        "*" = "Each hesper priority must be unique per respondent."
      ))
    }
  }
)
