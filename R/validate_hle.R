#' Validate SL (skip logic) list in HesperListEnhanced
#'
#' @typed sl_list: list[SL]
#'   The SL list to validate
#' @typed hl_hesper_vars: character[1+]
#'   Character vector of hesper variables from hesper_list
#' @typed other_list: list[vec]
#'   The other_list for subset variable validation
#' @typedreturn NULL
#'   NULL if valid, otherwise aborts with error
#' @keywords internal
validate_hle_sl <- function(sl_list, hl_hesper_vars, other_list) {
  if (length(sl_list) > 0) {
    check_vector_class(sl_list, SL, 'SL', use_S7_inherits = TRUE)
    purrr::map(sl_list, function(x) {
      SL@validator(x)
    })
    sl_hesper_vars <- purrr::map_chr(sl_list, \(x) x@hesper_var)
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

    # Validate other_list if provided
    sl_subset_vars <- purrr::map_chr(sl_list, \(x) x@subset_var)
    other_list_names <- names(other_list)
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

  NULL
}

#' Validate other_list length consistency in HesperListEnhanced
#'
#' @typed other_list: list[vec]
#'   The other_list to validate
#' @typed hesper_list: list[HesperVector]
#'   The hesper_list for length comparison
#' @typedreturn NULL
#'   NULL if valid, otherwise aborts with error
#' @keywords internal
validate_hle_other_list <- function(other_list, hesper_list) {
  if (length(other_list) > 0) {
    len_hl <- purrr::map_int(hesper_list, \(x) {
      length(x@hesper_vals)
    }) |>
      unique()
    other_list_lengths <- purrr::map_int(other_list, \(x) {
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

#' Validate priority_list in HesperListEnhanced
#'
#' @typed priority_list: list[HesperPriorities]
#'   The priority_list to validate
#' @typed hesper_list: list[HesperVector]
#'   The hesper_list for validation
#' @typed hl_hesper_vars: chr
#'   Character vector of hesper variables from hesper_list
#' @typedreturn NULL
#'   NULL if valid, otherwise aborts with error
#' @keywords internal
validate_hle_priority_list <- function(
  priority_list,
  hesper_list,
  hl_hesper_vars
) {
  if (length(priority_list) > 0) {
    # Validate the HesperPriorities object itself
    check_vector_class(
      priority_list,
      HesperPriorities,
      'priority_list',
      use_S7_inherits = TRUE
    )
    HesperPriorities@validator(priority_list[[1]])

    # Check length consistency with hesper_list
    if (length(hesper_list) > 0) {
      hesper_list_length <- length(hesper_list[[1]]@hesper_vals)
      priority_lengths <- c(
        length(priority_list[[1]]@top1),
        length(priority_list[[1]]@top2),
        length(priority_list[[1]]@top3)
      )
      if (!all(priority_lengths == hesper_list_length)) {
        rlang::abort(c(
          "HesperPriorities vectors must have the same length as hesper_list items.",
          "i" = glue::glue(
            "Expected length: {hesper_list_length}, ",
            "but got lengths: {unique(priority_lengths)}"
          )
        ))
      }

      # get all priority variables as lists for each priority level
      priority_lists <- list(
        top1 = priority_list[[1]]@top1,
        top2 = priority_list[[1]]@top2,
        top3 = priority_list[[1]]@top3
      )

      # validation across all positions
      invalid_priorities <- purrr::imap(
        seq_len(hesper_list_length),
        \(pos, idx) {
          purrr::imap(
            priority_lists,
            \(priority_vec, priority_name) {
              priority_var <- priority_vec[pos]

              if (is.na(priority_var)) {
                return(NULL)
              }

              var_index <- match(priority_var, hl_hesper_vars)

              if (is.na(var_index)) {
                return(list(
                  property = priority_name,
                  position = pos,
                  variable = priority_var,
                  error = "not_in_hesper_list"
                ))
              }

              actual_value <- hesper_list[[var_index]]@hesper_vals[pos]
              if (!is.na(actual_value) && actual_value != "serious_problem") {
                return(list(
                  property = priority_name,
                  position = pos,
                  variable = priority_var,
                  error = "not_serious_problem",
                  actual_value = actual_value
                ))
              }

              return(NULL)
            }
          ) |>
            purrr::discard(is.null)
        }
      ) |>
        purrr::flatten()

      # report validation errors if any
      if (length(invalid_priorities) > 0) {
        # Create error messages from structured data
        error_messages <- purrr::map_chr(
          invalid_priorities,
          \(err) {
            if (err$error == "not_in_hesper_list") {
              glue::glue(
                "Position {err$position}: {err$property} = '{err$variable}' is not present in hesper_list"
              )
            } else {
              glue::glue(
                "Position {err$position}: {err$property} = '{err$variable}' is not marked as 'serious_problem' (actual value: '{err$actual_value}')"
              )
            }
          }
        )

        rlang::abort(c(
          "Some hesper priorities do not correspond to items marked as 'serious_problem' at the same position.",
          "i" = "The following priority properties have invalid values:",
          stats::setNames(error_messages, rep("*", length(error_messages))),
          "!" = "Each priority must be marked as 'serious_problem' at the same respondent position."
        ))
      }
    }
  }

  NULL
}


#' Validate category_list in HesperListEnhanced
#'
#' @typed category_list: list[HesperCategories]
#'   The category_list to validate
#' @typed hl_hesper_vars: chr
#'   Character vector of hesper variables from hesper_list
#' @typedreturn NULL
#'   NULL if valid, otherwise aborts with error
#' @keywords internal
validate_hle_category_list <- function(category_list, hl_hesper_vars) {
  # Must be of length 0 or 1
  checkmate::assert_list(category_list, max.len = 1)

  if (length(category_list) > 0) {
    # Validate the HesperCategories object itself
    check_vector_class(
      category_list,
      HesperCategories,
      'category_list',
      use_S7_inherits = TRUE
    )
    HesperCategories@validator(category_list[[1]])

    # Check that all HESPER variables in categories are present in hesper_list
    category_hesper_vars <- purrr::map(
      category_list[[1]]@category_list,
      \(x) x@vars
    ) |>
      unlist(use.names = FALSE)
    missing_category_vars <- setdiff(category_hesper_vars, hl_hesper_vars)
    if (length(missing_category_vars) > 0) {
      rlang::abort(c(
        "All HESPER variables in category_list must be present in hesper_list.",
        "i" = glue::glue(
          "The following hesper_vars from category_list are missing in hesper_list: ",
          glue::glue_collapse(
            missing_category_vars,
            sep = ", ",
            last = " and "
          )
        )
      ))
    }
  }

  NULL
}
