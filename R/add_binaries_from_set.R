#' Add as many binaries than input columns, taking the value 1 if value in each column is inside a set a values
#' 
#' Useful to create a set of dummy variables from a set of categorical variables if they are in a defined set of choices
#' For HESPER package, can be used to create binaries for all HESPER items select one columns, to either record if it's a serious problem, undefined, serious problem with subset
#' Additionnal argument can be used to add a suffix to the column names of the binary created

#' @param df A df.table or df.frame: The df to be processed.
#' @param cols_character A character vector: The name of the columns that will be used to create binary columns.
#' @param value_1 A character vector: Set of values for which binary columns will be populated with 1 if `cols_character` is inside this set.
#' @param value_0 A character vector: Set of values for which binary columns will be populated with 0 if `cols_character` is inside this set.
#' @param value_na A character vector: Set of values for which binary columns will be populated with NA if `cols_character` is inside this set. 
#' @param value_default A character vector: Set of values for which binary columns will be populated with otherwise. Will be overriden with 0 if value_0 = NULL and with NA if value_na = NULL
#' @param suffix A character vector: The suffix to be added to the added binary vars. If NULL, vars are replaced by binary vars.
#' @param sep A character vector: separator used to add name suffix to original column names
#' @return A df.table or df.frame with the binary columns added.    
#' 
#' @importFrom data.table `:=`
#' @importFrom data.table `.SD`
#' 
#' @export
#' 
# #' @examples
# #' hesper_vars <- c("hesper_drinking_water", "hesper_food", "hesper_shelter", "hesper_toilet", "hesper_clean", "hesper_clothes_etc", "hesper_income_livelihood",
# #'                  "hesper_health", "hesper_distress", "hesper_safety", "hesper_education", "hesper_care", "hesper_support", "hesper_separation", "hesper_displaced", "hesper_information", "hesper_aid",
# #'                  "hesper_respect", "hesper_movement", "hesper_time", "hesper_law", "hesper_gbv", "hesper_drug", "hesper_mental_health", "hesper_care_community")
# #'   
# #' hesper_dat_comp <- hesper_dat |> 
# #'      ## Add HESPER binaries - global => prevalence of serious problem on all sample (regardless of subset or cleaning or undefined)
# #'      add_val_in_set_binaries(cols_character = hesper_vars, 
# #'                          value_1 = c("serious_problem"),
# #'                          value_0 = NULL,
# #'                          value_na = NULL,
# #'                          value_default = 0,
# #'                          replace = F, 
# #'                          suffix = "binary", 
# #'                          sep = ".") %>%
# #'      ## Add HESPER binaries taking subset into account [only respondents that reported either serious or not serious problem]
# #'      add_val_in_set_binaries(cols_character = hesper_vars, 
# #'                              value_1 = c("serious_problem"),
# #'                              value_0 = c("no_serious_problem"),
# #'                              value_na = NULL,
# #'                              value_default = NA_integer_,
# #'                              replace = F, 
# #'                              suffix = "binary_subset", 
# #'                              sep = ".") %>%
# #'      ## Add HESPER binaries for undefined values [any respondent in subset that chose not reply / dnk, pnta or reported not applicable choices]
# #'      add_val_in_set_binaries(cols_character = hesper_vars, 
# #'                              value_1 = c("pnta", "dnk", "not_applicable"),
# #'                              value_0 = NULL,
# #'                              value_na = NULL,
# #'                              value_default = 0,
# #'                              replace = F, 
# #'                              suffix = "binary_undefined", 
# #'                              sep = ".")
add_binaries_from_set <- function(
  df,
  vars,
  vals_0,
  vals_1,
  vals_na = NULL,
  default_val = NULL,
  suffix = NULL,
  sep = "."
) {

  #------ Checks

  # df is a data.frame
  checkmate::assert_data_frame(df)

  # df is not data.table, convert it
  if (!checkmate::testDataTable(df)) {
    rlang::warn("Converting df to data.table.")
    data.table::setDT(df)
  }

  # vars is a character vector
  checkmate::assertCharacter(vars, min.chars = 1, any.missing = FALSE)

  # vars are in df
  check_vars_in_df(df, vars)

  # vars are of class character
  check_vars_class_in_df(df, vars, "character")

  # are all vars in set 
  # TO DO:
  # should we check if all vars are in set when vals_na is not null?
  #check_vars_in_vals(df, vars, c(vals_0, vals_1, vals_na))

  # default value is integer
  checkmate::assertInteger(default_val, len = 1, null.ok = TRUE)

  # suffix is a character scalar or NULL
  checkmate::assertCharacter(suffix, len = 1, null.ok = TRUE)
    
  # if value_0 is NULL, and default_val is not 0 or is NA_integer_ ensure that defaut_val is overridden to 0 
  if (is.null(vals_0) && !(default_val %in% c(0, NA_integer_))) {
    default_val <- 0
    rlang::warn("vals_0 is NULL, default_val will be overridden to 0.")
  }
  
  #------ Compose

  # Prepare new variable names and warn if replacement
  if (!is.null(suffix)) { 
    new_vars <- paste0(vars, sep, suffix)
  } else {
    new_vars <- vars
  }
  warn_replace(df, new_vars)

  # Add binary variables
  df[, 
    (new_vars) := lapply(.SD, \(x) data.table::fcase(
      x %in% vals_1, 1L,
      x %in% vals_0, 0L,   
      x %in% vals_na, NA_integer_, 
      default = default_val)),
    .SDcols = vars]
  
  return(df)
  
}    
