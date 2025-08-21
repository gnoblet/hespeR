#' Get cleaned hesper_list after applying skip logic
#'
#' @param self A HesperListEnhanced object
#' @return The cleaned hesper_list
#' @export
apply_hesper_list_sl <- function(self) {
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
  hesper_list_cleaned <- c(hesper_list_cleaned, other_hesper_vecs)

  # Reorder the cleaned list to match the original order of hesper_list
  hesper_list_cleaned <- hesper_list_cleaned[match(hesper_list_vars, names(hesper_list_cleaned))]

  # Add it back to the HesperListEnhanced object
  self@hesper_list <- hesper_list_cleaned

  return(self)
}
