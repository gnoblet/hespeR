
#' @title Check if variables are in data frame
#' 
#' @param df A data frame
#' @param vars A vector of variable names
#' 
#' @return A stop statement
check_vars_in_df <- function(df, vars) {
  vars_nin <- setdiff(vars, colnames(df))
  if (length(vars_nin) > 0) {
    rlang::abort(glue::glue("Variables ", glue::glue_collapse(vars_nin, sep = ", ", last = ", and "), " not found in data frame."))
  }
}


#' @title Check for duplicate values
#' 
#' @param vec A vector
#' @param msg A message
#' 
#' @return A stop statement
check_dupes <- function(vec, msg) {
  dupes_vec <- unique(vec[duplicated(vec)])
  if(length(dupes_vec) > 0){
    rlang::abort(
      glue::glue(
        msg,
        glue::glue_collapse(dupes_vec, sep = ", ", last = ", and "),    
      )
    )
  }
}

#' @title Check if variables are of any given class(es)
#' 
#' @param df A data frame
#' @param vars A vector of variable names
#' @param class A vector of classes
#' 
#' @return A stop statement
check_vars_class_in_df <- function(df, vars, class) {
  vars_not_class <- vars[!sapply(df[, ..vars], function(x) checkmate::testClass(x, class))]
  if (length(vars_not_class) > 0) {
    rlang::abort(glue::glue(
      "Variables ", 
      glue::glue_collapse(vars_not_class, sep = ", ", last = ", and "), 
      " are not of class ",
      glue::glue_collapse(class, sep = ", ", last = ", or "),
      "."))
  }
}

#' @title Check if lists of subset are in the right format
#' 
#' @param sv_l A list of subset dictionaries
#' 
#' @return A stop statement
check_sv_l <- function(sv_l, df, hesper_vars) {
    
  sv_l_assess <- list()
  
  for (sv_el in names(sv_l)) {

    # Get sublist
    sublist <- sv_l[[sv_el]]

    # sublist are lists
    if (!is.list(sublist)) {
      return(paste(sv_el, "is not a list"))
    }
    # sublist is of length 3
    if (length(sublist) != 3) {
      rlang::abort(c(
        paste("Subset dictionary", sv_el, "does not have exactly 3 items."),
        "*" = paste("Subset dictionary", sv_el, "has", length(sublist), "item(s)."),
        "i" = "It should consist of hesper_vars (vector of hesper variables), subset_var (variable to use for subset) and subset_vals (vector of subset values corresponding to subset_var)."
      ))
    }

    # sublist items are named correctly
    sublist_names <- names(sublist)[!(names(sublist) %in% c("hesper_vars", "subset_var", "subset_vals"))]
    if (length(sublist_names) > 0) {
      rlang::abort(c(
          paste("Subset dictionary", sv_el, "does not have correctly named items"),
          "*" = paste("Subset dictionary", sv_el, "has the following wrongly named item(s):", paste0(sublist_names, collapse = ", "), "."),
          "i" = "It should consist of hesper_vars (vector of hesper variables), subset_var (variable to use for subset) and subset_vals (vector of subset values corresponding to subset_var)."
      ))
    }

    # sublist element "hesper_vars is a character vector and one of hesper_vars
    checkmate::assertCharacter(sublist[["hesper_vars"]], min.chars = 1)
    hesper_vars_names <- sublist[["hesper_vars"]][!(sublist[["hesper_vars"]] %in% hesper_vars)]
    if (length(hesper_vars_names) > 0) {
      rlang::abort(c(
        paste("Subset dictionary", sv_el, "has variables in item 'hesper_vars' that are not in param 'hesper_vars'"),
        "*" = "Subset dictionary", sv_el, "has the following variables in item 'hesper_vars' that are not in param 'hesper_vars':", paste0(hesper_vars_names, collapse = ", "),
        "i" = "Item 'hesper_vars' should only contain variables that you inputed in param 'hesper_vars'."
      ))
    }

    # sublist element "subset_var" is a character scalar and belong in df
    checkmate::assertCharacter(sublist[["subset_var"]], len = 1, min.chars = 1)
    if (!(sublist[["subset_var"]] %in% colnames(df))) {
      rlang::abort(c(
        paste("Subset dictionary", sv_el, "has variable in item 'subset_var' that is not in df"),
        "*" = paste("Subset dictionary", sv_el, "has the following variable in item 'subset_var' that is not in df:", sublist[["subset_var"]], "."),
        "i" = "Item 'subset_var' should only contain variables that are in df."
      ))
    }

    # sublist element "subset_vals" is a character vector without NA or NULL values
    checkmate::assertCharacter(sublist[["subset_vals"]], min.chars = 1, null.ok = FALSE, any.missing = FALSE)

    # warn if there is no occurence of "subset_vals" in "subset_var" in df
    if (!any(sublist[["subset_vals"]] %in% df[[sublist[["subset_var"]]]])) {
      rlang::warn(paste("For subset dictionary", sv_el, "no'subset_vals' exist in 'subset_var' in df"))
    }

  }
}






#' @title Stop statement values are not in set
#'
#' @param df A data frame
#' @param vars A vector of column names (quoted)
#' @param set A vector of values
#' @param main_message A main message
#'
#' @return A stop statement
check_vars_in_set <- function(df, vars, set, main_message = "All columns must be in the following set: "){

  #------ Check for missing columns
  check_vars_in_df(df, vars)

  #------ Values not in set
  values_lgl <- purrr::map_lgl(
    dplyr::select(
      df,
      dplyr::all_of(vars)
    ),
    \(x) {
      !all(stats::na.omit(unique(x)) %in% set)
    }
  )

  if (any(values_lgl)) {

    vars <- vars[values_lgl]
    values_chr <- names(values_lgl)

    # Get values not in set
    df_vars <- dplyr::select(df, dplyr::all_of(vars))
    values_chr <- purrr::map(df_vars, \(x) {
      x <- unique(x)
      x[!is.na(x) & !(x %in% set)]
    })

    values_chr <- purrr::imap_chr(values_chr, \(x, idx) {
      glue::glue("{idx}: {glue::glue_collapse(x, sep = ', ', last = ' and ')}")
    })

    rlang::abort(c(
      glue::glue(main_message, glue::glue_collapse(set, sep = ", ")),
      "i" = glue::glue(
        "The following columns have values out of the set Please check.\n",
        glue::glue_collapse(vars, sep = "\n")
      ),
      "x" = glue::glue("The values out of the set are:\n", glue::glue_collapse(values_chr, sep = "\n"))
    ))
  }

  return(TRUE)
}
