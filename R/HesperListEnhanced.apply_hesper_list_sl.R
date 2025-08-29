#' Apply SL cleaning to HesperListEnhanced object
#'
#' @typed self: HesperListEnhanced
#'  A HesperListEnhanced object
#'
#' @typedreturn HesperListEnhanced
#'  The cleaned HesperListEnhanced object after applying skip logic
#'
#' @export
apply_hesper_list_sl <- S7::new_generic(
  "apply_hesper_list_sl",
  dispatch_args = c("self"),
  function(self) {
    S7::S7_dispatch()
  }
)

#' @rdname apply_hesper_list_sl
#' @name apply_hesper_list_sl
#'
S7::method(apply_hesper_list_sl, HesperListEnhanced) <- function(self) {
  # HesperListEnhanced is a HesperListEnhanced
  # S7::S7_inherits(self, HesperListEnhanced)
  # HesperListEnhanced@validator(self)

  # property is a single character string in the allowed set
  if (length(self@SL) == 0) {
    rlang::warn(c(
      "No skip logic rules (SL) defined in the HesperListEnhanced object.",
      "i" = "Skipping application of skip logic rules."
    ))

    return(self)
  }

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

  # Add it back to the HesperListEnhanced object
  self@hesper_list <- hesper_list_cleaned

  return(self)
}

#' Apply SL cleaning to HesperPriorities object
#'
#' @typed self: HesperListEnhanced
#'  A HesperListEnhanced object
#' @typed cascading: character[1]
#'  Cascading behavior when priorities are removed. Either "missing" (default) or "upcycle".
#'  - "missing": removed priorities become NA, remaining priorities stay in place
#'  - "upcycle": remaining priorities cascade up to fill gaps left by removed priorities
#'
#' @typedreturn HesperListEnhanced
#'  The HesperListEnhanced object with cleaned priorities after applying skip logic
#'
#' @export
apply_priority_list_sl <- S7::new_generic(
  "apply_priority_list_sl",
  dispatch_args = c("self"),
  function(self, cascading = "missing") {
    S7::S7_dispatch()
  }
)

#' @rdname apply_priority_list_sl
#' @name apply_priority_list_sl
#'
S7::method(apply_priority_list_sl, HesperListEnhanced) <- function(
  self,
  cascading = "missing"
) {
  # Validate cascading parameter
  checkmate::assert_subset(cascading, c("missing", "upcycle"))

  # Check if skip logic rules exist
  if (length(self@SL) == 0) {
    rlang::warn(c(
      "No skip logic rules (SL) defined in the HesperListEnhanced object.",
      "i" = "Skipping application of skip logic rules to priorities."
    ))
    return(self)
  }

  # Check if priorities exist
  if (length(self@priority_list) == 0) {
    rlang::warn(c(
      "No priority_list defined in the HesperListEnhanced object.",
      "i" = "Skipping application of skip logic rules to priorities."
    ))
    return(self)
  }

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
  self@priority_list <- list(
    HesperPriorities(
      top1 = top1_cleaned,
      top2 = top2_cleaned,
      top3 = top3_cleaned,
      allow_missing = TRUE
    )
  )

  return(self)
}

#' Apply SL cleaning to both hesper_list and hesper_priorities
#'
#' @typed self: HesperListEnhanced
#'  A HesperListEnhanced object
#' @typed cascading: character[1]
#'  The cascading behavior for missing values. Options are "missing" or "upcycle". See [apply_priority_list_sl()].
#'
#' @typedreturn HesperListEnhanced
#'  The cleaned HesperListEnhanced object after applying skip logic to both components
#'
#' @details
#'  This method applies skip logic rules to both the hesper_list and hesper_priorities
#'  components of a HesperListEnhanced object. It sequentially calls:
#'  1. [apply_hesper_list_sl()] to clean the HESPER response data
#'  2. [apply_priority_list_sl()] to clean the priority rankings
#'
#'  This ensures consistency between the HESPER responses and priority data -
#'  if a respondent shouldn't answer a particular HESPER item due to skip logic,
#'  that item is also removed from their priority rankings.
#'
#' @export
apply_sl <- S7::new_generic(
  "apply_sl",
  dispatch_args = c("self"),
  function(self, cascading = "missing") {
    S7::S7_dispatch()
  }
)

#' @rdname apply_sl
#' @name apply_sl
#'
S7::method(apply_sl, HesperListEnhanced) <- function(
  self,
  cascading = "missing"
) {
  # Apply skip logic to hesper_list first
  self <- apply_hesper_list_sl(self)

  # Then apply skip logic to hesper_priorities
  self <- apply_priority_list_sl(self, cascading = cascading)

  return(self)
}
