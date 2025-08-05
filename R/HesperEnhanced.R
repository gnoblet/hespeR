#' S7 class for hespeR data.frames (enhanced)
#'
#' @typed df: data.frame[,1+]
#'   A data frame containing HESPER items, HESPER response options, and optionally further columns.
#' @typed sl_list: list[SL]
#'   Optional list of SL (skip logic) S7 objects, each describing a skip logic rule for the data. See details for input validation.
#' @typed extra_cols: character{0+}
#'   Optional character vector of additional column names in the data frame (e.g., population group, site, etc.).
#'
#' @description
#' The `HesperEnhanced` class extends `HesperDefault` to support enhanced HESPER data frames.
#' It allows for additional columns beyond the standard HESPER variables, and supports skip logic rules via a list of S7 `SL` objects.
#'
#' @details
#' - Inherits all properties and validation from `HesperDefault`.
#' - Allows for extra columns in the data frame, such as population group, site, or other metadata.
#' - Optionally accepts a list of S7 `SL` objects, each representing a skip logic rule.
#' - The validator checks:
#'   - All extra columns (if specified or inferred) exist in the data frame.
#'   - For `sl_list`:
#'     - Must be a list of S7 `SL` objects.
#'     - For each SL object:
#'       - `subset_var` exists in the extra columns (not in HESPER variables).
#'       - The type of `subset_var` in the data frame matches the type expected by the SL object (using `typeof` and factor checks).
#'     - No duplicate `subset_var` or `hesper_var` within the SL list.
#' - Additional columns are simply part of the data frame; no further validation is performed on their values.
#'
#' @export
HesperEnhanced <- S7::new_class(
  "HesperEnhanced",
  parent = HesperDefault,
  properties = list(
    sl_list = S7::new_property(class = S7::class_list, default = NULL),
    extra_cols = S7::new_property(class = S7::class_character, default = NULL)
  ),
  validator = function(self) {
    # Infer extra columns if not provided
    hesper_vars <- hesper_vars
    df_cols <- colnames(self@df)
    # Infer extra columns if not provided
    extra_cols <- self@extra_cols
    if (is.null(extra_cols)) {
      extra_cols <- setdiff(df_cols, hesper_vars)
    }
    # Validate extra columns are present in df
    missing_cols <- setdiff(extra_cols, df_cols)
    if (length(missing_cols) > 0) {
      return(paste0(
        "Missing extra columns in df: ",
        paste(missing_cols, collapse = ", ")
      ))
    }
    # Validate SL list
    if (!is.null(self@sl_list)) {
      # Check that all elements are S7 SL objects
      check_list_class(
        self@sl_list,
        class = "SL"
      )

      # Check for duplicates in subset_var and hesper_var
      subset_vars <- purrr::map_chr(self@sl_list, \(x) x@subset_var)
      hesper_vars_sl <- purrr::map_chr(self@sl_list, \(x) x@hesper_var)
      check_dupes(subset_vars)
      check_dupes(hesper_vars_sl)

      # Check subset_var exists in extra_cols (not hesper_vars)
      for (sl in self@sl_list) {
        sv <- sl@subset_var
        if (!sv %in% extra_cols) {
          return(paste0(
            "subset_var '",
            sv,
            "' in SL not found in extra columns of df."
          ))
        }
        # Check type matches (typeof and factor)
        df_col <- self@df[[sv]]
        sl_vals <- sl@subset_vals
        if (is.factor(df_col)) {
          if (!is.character(sl_vals) && !is.factor(sl_vals)) {
            return(paste0(
              "Type mismatch for subset_var '",
              sv,
              "': df column is factor, SL subset_vals is ",
              class(sl_vals)[1]
            ))
          }
        } else {
          if (typeof(df_col) != typeof(sl_vals)) {
            return(paste0(
              "Type mismatch for subset_var '",
              sv,
              "': df type is ",
              typeof(df_col),
              ", SL subset_vals type is ",
              typeof(sl_vals)
            ))
          }
        }
      }
    }
    NULL
  }
)
