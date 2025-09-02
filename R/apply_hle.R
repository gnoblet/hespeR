#' Apply skip logic to hesper_list (helper function for getter)
#'
#' @typed self: HesperListEnhanced
#'   A HesperListEnhanced object
#'
#' @typedreturn list[HesperVector]
#'   The cleaned hesper_list
#'
#' @keywords internal
apply_hle_hesper_list_sl <- function(self) {
  #------ Checks

  # Validate other_list
  validate_hle_other_list(self@other_list, self@hesper_list)

  # Validate SL if provided
  hl_hesper_vars <- purrr::map_chr(self@hesper_list, \(x) x@hesper_var)
  validate_hle_sl(self@SL, hl_hesper_vars, self@other_list)

  # if no SL provided, return original hesper_list
  if (length(self@SL) == 0) {
    return(self@hesper_list)
  }

  #------ Apply SL cleaning

  # Get idx of hesper_list where skip logic rules apply
  sl_hesper_vars <- purrr::map_chr(self@SL, \(x) x@hesper_var)
  hesper_list_vars <- purrr::map_chr(self@hesper_list, \(x) x@hesper_var)

  hesper_list_cleaned <- list()

  for (hesper_var in sl_hesper_vars) {
    # Get the hesper vector that matches the current hesper_var
    hesper_vec <- self@hesper_list[which(
      purrr::map_chr(self@hesper_list, \(x) x@hesper_var) == hesper_var
    )]
    hesper_vals <- hesper_vec[[1]]@hesper_vals

    # Get the SL that matches the current hesper_var
    sl <- self@SL[which(
      purrr::map_chr(self@SL, \(x) x@hesper_var) == hesper_var
    )]
    sl_subset_var <- sl[[1]]@subset_var
    sl_subset_vals <- sl[[1]]@subset_vals

    # Get the subset data from other_list
    subset_data <- self@other_list[[sl_subset_var]]
    # Find indices to clean based on subset data
    hesper_vals[subset_data %in% sl_subset_vals] <- NA

    hesper_list_cleaned[[hesper_var]] <- HesperVector(
      hesper_var = hesper_var,
      hesper_vals = hesper_vals,
      allow_missing = TRUE
    )
  }

  # Add any hesper vectors that do not have skip logic rules applied
  other_hesper_vars <- setdiff(hesper_list_vars, sl_hesper_vars)
  other_hesper_vecs <- self@hesper_list[which(
    hesper_list_vars %in% setdiff(hesper_list_vars, sl_hesper_vars)
  )]

  # set names for other hesper
  names(other_hesper_vecs) <- other_hesper_vars
  hesper_list_cleaned <- c(hesper_list_cleaned, other_hesper_vecs)

  # Reorder the cleaned list to match the original order of hesper_list
  hesper_list_cleaned <- hesper_list_cleaned[match(
    hesper_list_vars,
    names(hesper_list_cleaned)
  )]

  return(hesper_list_cleaned)
}

#' Apply skip logic to priority_list (helper function for getter)
#'
#' @typed self: HesperListEnhanced
#'   A HesperListEnhanced object
#' @typed cascading: character[1]
#'  Cascading method to use when cleaning priorities. Options are: "upcycle" (default - if higher priority is NA, move lower priorities up) or "missing" (if higher priority is NA, cascade NA down).
#'
#' @typedreturn list[HesperPriorities]
#'   The cleaned priority_list
#'
#' @keywords internal
apply_hle_priority_list_sl <- function(self, cascading = "upcycle") {
  #------ Checks

  # Validate other_list
  validate_hle_other_list(self@other_list, self@hesper_list)

  # Validate SL if provided
  hl_hesper_vars <- purrr::map_chr(self@hesper_list, \(x) x@hesper_var)
  validate_hle_sl(self@SL, hl_hesper_vars, self@other_list)

  # Validate priority_list
  validate_hle_priority_list(
    self@priority_list,
    self@hesper_list,
    hl_hesper_vars
  )

  # if no SL provided, return original priority_list
  if (length(self@SL) == 0 || length(self@priority_list) == 0) {
    return(self@priority_list)
  }

  #------ Apply SL cleaning

  # Apply skip logic rules to priorities
  top1_cleaned <- self@priority_list[[1]]@top1
  top2_cleaned <- self@priority_list[[1]]@top2
  top3_cleaned <- self@priority_list[[1]]@top3

  # Apply each skip logic rule
  for (sl in self@SL) {
    hesper_var <- sl@hesper_var
    subset_var <- sl@subset_var
    subset_vals <- sl@subset_vals

    # Get the subset data
    subset_data <- self@other_list[[subset_var]]

    # Find indices where skip logic applies
    affected_indices <- which(subset_data %in% subset_vals)

    # For affected respondents, remove this hesper_var from their priorities
    if (length(affected_indices) > 0) {
      top1_cleaned[affected_indices][
        top1_cleaned[affected_indices] == hesper_var
      ] <- NA
      top2_cleaned[affected_indices][
        top2_cleaned[affected_indices] == hesper_var
      ] <- NA
      top3_cleaned[affected_indices][
        top3_cleaned[affected_indices] == hesper_var
      ] <- NA
    }
  }

  # Apply cascading logic based on cascading parameter
  if (cascading == "upcycle") {
    for (i in seq_along(top1_cleaned)) {
      # Collect non-NA priorities for this respondent
      priorities <- c(top1_cleaned[i], top2_cleaned[i], top3_cleaned[i])
      non_na_priorities <- priorities[!is.na(priorities)]

      # Reassign priorities, filling from top1 down
      if (length(non_na_priorities) >= 1) {
        top1_cleaned[i] <- non_na_priorities[1]
      } else {
        top1_cleaned[i] <- NA_character_
      }

      if (length(non_na_priorities) >= 2) {
        top2_cleaned[i] <- non_na_priorities[2]
      } else {
        top2_cleaned[i] <- NA_character_
      }

      if (length(non_na_priorities) >= 3) {
        top3_cleaned[i] <- non_na_priorities[3]
      } else {
        top3_cleaned[i] <- NA_character_
      }
    }
  } else if (cascading == "missing") {
    # Apply cascading NA logic - if higher priority is NA, cascade NA down
    for (i in seq_along(top1_cleaned)) {
      # If top1 is NA, cascade NA to top2 and top3
      if (is.na(top1_cleaned[i])) {
        top2_cleaned[i] <- NA_character_
        top3_cleaned[i] <- NA_character_
      } else if (is.na(top2_cleaned[i])) {
        # If top2 is NA (and top1 is not), cascade NA to top3
        top3_cleaned[i] <- NA_character_
      }
      # If only top3 is NA, it stays NA (no cascading needed)
    }
  }

  # Create new HesperPriorities object with cleaned data
  hp <- list(
    HesperPriorities(
      top1 = top1_cleaned,
      top2 = top2_cleaned,
      top3 = top3_cleaned,
      allow_missing = TRUE
    )
  )

  return(hp)
}


#' Create category-level HESPER vectors from HesperListEnhanced
#'
#' @typed self: HesperListEnhanced
#'   A HesperListEnhanced object.
#'
#' @typedreturn list[character]
#'   A named list of character vectors representing category-level aggregation (including binaries).
#'
#' @details
#' This function creates category-level HESPER vectors by aggregating individual
#' HESPER variables within each category using hierarchical logic:
#' - "serious_problem" if any variable is "serious_problem"
#' - "pnta" if any variable is "pnta" (and none are "serious_problem")
#' - "dnk" if any variable is "dnk" (and none are "serious_problem" or "pnta")
#' - "no_serious_problem" if ALL variables are "no_serious_problem"
#' - "not_applicable" if ALL variables are "not_applicable"
#'
#' @keywords internal

apply_hle_hesper_list_cat <- function(
  self
) {
  #------ Checks

  # Validate category list
  validate_hle_category_list(
    self@category_list,
    purrr::map_chr(self@hesper_list, \(x) x@hesper_var)
  )
  # if no category_list provided, return empty list
  if (length(self@category_list) == 0) {
    return(list())
  }
  #------ Create category-level HESPER vectors

  hesper_categories <- self@category_list[[1]]
  hesper_list <- self@hesper_list

  purrr::map(hesper_categories@category_list, \(x) {
    cat_name = x@cat
    cat_vars = x@vars

    hesper_list_sub <- hesper_list[which(
      purrr::map_chr(hesper_list, \(y) y@hesper_var) %in% cat_vars
    )]

    # pmap over hesper_list_sub and extract hesper_vals
    hesper_vals_list <- purrr::map(hesper_list_sub, \(y) y@hesper_vals)
    hesper_vals_list <- purrr::pmap_chr(hesper_vals_list, \(...) {
      vals <- c(...)

      # logic is:
      # if any serious_problem -> serious_problem
      # if all no_serious_problem -> no_serious_problem
      # if any is dnk, dnk
      # if any is pnta, pnta
      # if any is not_applicable, not_applicable
      res <- if ("serious_problem" %in% vals) {
        "serious_problem"
      } else if (all(vals == "no_serious_problem")) {
        "no_serious_problem"
      } else if ("dnk" %in% vals) {
        "dnk"
      } else if ("pnta" %in% vals) {
        "pnta"
      } else if ("not_applicable" %in% vals) {
        "not_applicable"
      } else {
        NA_character_
      }
    })

    # let's add binaries for each hesper_opts
    hesper_vals_bins <- create_binary_vectors(hesper_vals_list, hesper_opts())

    return(list(
      vals = hesper_vals_list,
      bins = hesper_vals_bins
    ))
  }) |>
    purrr::set_names(purrr::map_chr(hesper_categories@category_list, \(x) {
      x@cat
    }))
}
