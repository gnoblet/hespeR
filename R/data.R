#' All allowed HESPER item column names
#'
#' A character vector listing all allowed HESPER item columns for use in hespeR data structures and validation or if needed to be accessed easily.
#'
#' @typed none: empty
#'  No input parameters.
#'
#' @typedreturn character[29]
#'  All allowed HESPER item names.
#'
#' @export
hesper_vars <- function() {
  c(
    "hesper_drinking_water",
    "hesper_food",
    "hesper_shelter",
    "hesper_toilet",
    "hesper_clean",
    "hesper_clean_female",
    "hesper_clothes_etc",
    "hesper_income_livelihood",
    "hesper_health",
    "hesper_health_care_male",
    "hesper_health_care_female",
    "hesper_distress",
    "hesper_safety",
    "hesper_education",
    "hesper_care",
    "hesper_support",
    "hesper_separation",
    "hesper_displaced",
    "hesper_information",
    "hesper_aid",
    "hesper_respect",
    "hesper_movement",
    "hesper_time",
    "hesper_law",
    "hesper_gbv",
    "hesper_drug",
    "hesper_mental_health",
    "hesper_care_community",
    "hesper_other"
  )
}

#' All allowed response options for HESPER items
#'
#' This character vector contains all allowed response options for HESPER items, which can be used for validation or reference in the hespeR package These are: 'serious_problem', 'no_serious_problem', 'dnk', 'pnta', and 'not_applicable'.
#' @typed none: empty
#'  No input parameters.
#'
#' @typedreturn character[5]
#'  All allowed HESPER response options.
#'
#' @export
hesper_opts <- function() {
  c(
    "serious_problem",
    "no_serious_problem",
    "dnk",
    "pnta",
    "not_applicable"
  )
}
