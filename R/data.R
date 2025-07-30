#' All possible HESPER item column names
#'
#' A character vector listing all possible HESPER item columns for use in hespeR data structures and validation.
#' @format character vector
#' @keywords internal
'hesper_vars'

#' Allowed HESPER response options
#'
#' A character vector of all allowed response options for HESPER items: 'serious_problem', 'no_serious_problem', 'dnk', 'pnta', and 'not_applicable'.
#' @format character vector
#' @keywords internal
"hesper_opts"

#' Some Hesper dummy data
#'
#' A dataset with hesper columns, for example and testing purposes.
#'
#' This data frame contains columns corresponding to all possible HESPER items, as well as possible grouping or metadata columns (e.g., pop_group). Values in HESPER columns are restricted to the allowed HESPER response levels.
#'
#' @format A data.frame with columns:
#'   \describe{
#'     \item{hesper_*}{Character columns for each HESPER item, e.g., hesper_drinking_water, hesper_food, ...}
#'     \item{pop_group}{(optional) Character column for population group, e.g., refugees, host, idp}
#'     \item{...}{Other possible metadata columns}
#'   }
#' @source Generated for hespeR package examples and tests.
"hesper_dat"
