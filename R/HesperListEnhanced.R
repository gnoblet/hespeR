#' S7 class for hespeR list of vectors with skip logic
#'
#' @typed hesper_list: list[HesperVector]
#'   A list of `HesperList` items, each representing a HESPER item and its allowed options.
#' @typed SL: list[SL]
#'   `SL` objects representing skip logic rules.
#' @typed other_list: list[vec]
#'   Other vectors associated with the HESPER list, typically used for skip logic rules or disaggregation.
#' @typed priorty_list: list[HesperPriorities]
#'   `HesperPriorities` list of length 1 representing top 3 priorities for respondents.  When provided, the validator will ensure that the priorities correspond to items that were indicated as serious problems in the HESPER list.
#'
#' @typedreturn S7_object
#'   A S7 object of class `HesperListEnhanced`, representing a list of HESPER vectors associated with skip logic rules, priorities, and additional data.
#'
#' @details
#'  This class extends the `HesperList` class to include support for skip logic (SL) rules, respondent priorities and additional data. Note that the only non empty list that must be provided is `hesper_list`. The other lists can be empty (length 0).
#'
#' It validates the following:
#'
#' - Each item in `SL` must be a valid `hespeR::SL` object.
#' - The `hesper_var` in each `SL` object must correspond to a `hesper_var` in `hesper_list`.
#' - The `subset_var` in each `SL` object must correspond to a name in `other_list`.
#' - All items in `other_list` must have the same length as items in `hesper_list`.
#' - If `priority_list` is provided, it must be a valid `HesperPriorities` object and its vectors must have the same length as items in `hesper_list`.
#' - Each priority in `priority_list` must correspond to a `hesper_var` in `hesper_list` that was marked as "serious_problem" at the same position.
#'
#' @export
HesperListEnhanced <- S7::new_class(
  "HesperListEnhanced",
  parent = HesperList,
  properties = list(
    hesper_list = S7::class_list,
    SL = S7::class_list,
    other_list = S7::class_list,
    priority_list = S7::class_list
  ),
  validator = function(self) {
    HesperList@validator(self)

    # get all hesper_vars in hesper_list
    hl_hesper_vars <- purrr::map_chr(self@hesper_list, \(x) x@hesper_var)

    # Validate SL if provided
    if (length(self@SL) > 0) {
      check_vector_class(self@SL, SL, 'SL', use_S7_inherits = TRUE)
      purrr::map(self@SL, function(x) {
        SL@validator(x)
      })
      sl_hesper_vars <- purrr::map_chr(self@SL, \(x) x@hesper_var)
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

    # Validate priority_list if provided
    # Must be of length 0 or 1
    checkmate::assert_list(self@priority_list, max.len = 1)
    if (length(self@priority_list) > 0) {
      # Validate the HesperPriorities object itself
      check_vector_class(
        self@priority_list,
        HesperPriorities,
        'priority_list',
        use_S7_inherits = TRUE
      )
      HesperPriorities@validator(self@priority_list[[1]])

      # Check length consistency with hesper_list
      if (length(self@hesper_list) > 0) {
        hesper_list_length <- length(self@hesper_list[[1]]@hesper_vals)
        priority_lengths <- c(
          length(self@priority_list[[1]]@top1),
          length(self@priority_list[[1]]@top2),
          length(self@priority_list[[1]]@top3)
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
          top1 = self@priority_list[[1]]@top1,
          top2 = self@priority_list[[1]]@top2,
          top3 = self@priority_list[[1]]@top3
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

                actual_value <- self@hesper_list[[var_index]]@hesper_vals[pos]
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
)
