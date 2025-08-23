#' All allowed HESPER item column names
#'
#' A character vector listing all allowed HESPER item columns for use in hespeR data structures and validation or if needed to be accessed easily.
#'
#' @typed ...: character[0,29]
#'  Optional variable names to validate. If none provided, all allowed variables are returned.
#'
#' @typedreturn character[29]
#'  All allowed HESPER item names.
#'
#' @export
hesper_vars <- function(...) {
  #------ Possible values

  allowed_vars <- c(
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

  #------ Checks

  vars <- list(...)

  # no duplicates allowed in ...
  if (length(vars) > 0) {
    # Check for duplicates
    if (any(duplicated(vars))) {
      dupes <- unique(vars[duplicated(vars)])
      rlang::abort(c(
        "Duplicated variable names provided",
        "i" = glue::glue(
          "Duplicated variable(s) found: ",
          glue::glue_collapse(dupes, sep = ", ")
        )
      ))
    }
  }

  # all elements are character strings
  checkmate::assert_list(vars, types = "character", max.len = 29)

  # all elements are in allowed_vars
  if (length(vars) > 0) {
    not_allowed <- setdiff(unlist(vars), allowed_vars)
    if (length(not_allowed) > 0) {
      rlang::abort(c(
        "Invalid variable names provided",
        "i" = glue::glue(
          "Invalid variable(s) found: ",
          glue::glue_collapse(not_allowed, sep = ", ")
        ),
        "*" = "Run hesper_vars() to get all allowed variables."
      ))
    }
  }

  #----- Get vars

  if (length(vars) == 0) {
    return(allowed_vars)
  } else {
    return(unlist(vars))
  }
}

#' All allowed response options for HESPER items
#'
#' This character vector contains all allowed response options for HESPER items, which can be used for validation or reference in the hespeR package These are: 'serious_problem', 'no_serious_problem', 'dnk', 'pnta', and 'not_applicable'.
#'
#' @typed ...: character[0,5]
#'  Optional options to validate. If none provided, all allowed options are returned.
#'
#' @typedreturn character[5]
#'  All allowed HESPER response options.
#'
#' @export
hesper_opts <- function(...) {
  #------ Possible values
  allowed_opts <- c(
    "serious_problem",
    "no_serious_problem",
    "dnk",
    "pnta",
    "not_applicable"
  )

  #------ Checks

  opts <- list(...)
  # no duplicates allowed in ...
  if (length(opts) > 0) {
    # Check for duplicates
    if (any(duplicated(opts))) {
      dupes <- unique(opts[duplicated(opts)])
      rlang::abort(c(
        "Duplicated options provided",
        "i" = glue::glue(
          "Duplicated option(s) found: ",
          glue::glue_collapse(dupes, sep = ", ")
        )
      ))
    }
  }

  # all elements are character strings
  checkmate::assert_list(opts, types = "character", max.len = 5)

  # all elements are in allowed_opts
  if (length(opts) > 0) {
    not_allowed <- setdiff(unlist(opts), allowed_opts)
    if (length(not_allowed) > 0) {
      rlang::abort(c(
        "Invalid options provided",
        "i" = glue::glue(
          "Invalid option(s) found: ",
          glue::glue_collapse(not_allowed, sep = ", ")
        ),
        "*" = "Run hesper_opts() to get all allowed options."
      ))
    }
  }

  #----- Get opts
  if (length(opts) == 0) {
    return(allowed_opts)
  } else {
    return(unlist(opts))
  }
}
