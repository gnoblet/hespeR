#' hespeR S7 Methods

#' Add a new column to a HesperDefault or HesperEnhanced object
add_hesper_column <- S7::new_generic("add_hesper_column", "x")

S7::method(add_hesper_column, HesperDefault) <- function(
  x,
  new_col_name,
  new_col_values
) {
  x@data[[new_col_name]] <- new_col_values
  S7::validate(x)
  x
}

S7::method(add_hesper_column, HesperEnhanced) <- function(
  x,
  new_col_name,
  new_col_values
) {
  x@data[[new_col_name]] <- new_col_values
  S7::validate(x)
  x
}

#' Apply skip logic to a HesperDefault or HesperEnhanced object using an SL object
apply_skip_logic <- S7::new_generic("apply_skip_logic", c("x", "sl"))

S7::method(apply_skip_logic, list(HesperDefault, SL)) <- function(x, sl) {
  dat <- x@data
  hesper_var <- sl@hesper_var
  subset_var <- sl@subset_var
  subset_vals <- sl@subset_vals
  if (!(hesper_var %in% names(dat))) {
    stop(paste0("hesper_var '", hesper_var, "' not found in data"))
  }
  if (!(subset_var %in% names(dat))) {
    stop(paste0("subset_var '", subset_var, "' not found in data"))
  }
  idx <- !(dat[[subset_var]] %in% subset_vals)
  dat[[hesper_var]][idx] <- NA
  x@data <- dat
  S7::validate(x)
  x
}

S7::method(apply_skip_logic, list(HesperEnhanced, SL)) <- function(x, sl) {
  dat <- x@data
  hesper_var <- sl@hesper_var
  subset_var <- sl@subset_var
  subset_vals <- sl@subset_vals
  if (!(hesper_var %in% names(dat))) {
    stop(paste0("hesper_var '", hesper_var, "' not found in data"))
  }
  if (!(subset_var %in% names(dat))) {
    stop(paste0("subset_var '", subset_var, "' not found in data"))
  }
  idx <- !(dat[[subset_var]] %in% subset_vals)
  dat[[hesper_var]][idx] <- NA
  x@data <- dat
  S7::validate(x)
  x
}
