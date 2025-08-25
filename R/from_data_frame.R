#' Convert data.frame to HesperList or HesperListEnhanced
#'
#' Returns a HesperList or HesperListEnhanced from a data.frame.
#'
#' @typed df: data.frame
#'  A data.frame containing the HESPER data.
#' @typed hesper_vars: character[1+]
#'  Names of HESPER item columns in df.
#' @typed allow_missing: logical[1]
#'  Whether to allow missing values in HESPER items. Default is TRUE.
#' @typed other_vars: character[1+] | NULL
#'  Names of other (non-HESPER) columns in df to include in HesperListEnhanced. If NULL (default), all non-HESPER columns are included.
#' @typed priority_vars: character[1+] | NULL
#' Names of priority columns in df. If provided, a HesperPriorities object will be created and included in the HesperListEnhanced. If NULL (default), no priorities are included. It must be in order [top1, top2, top3] if provided.
#'
#' @typedreturn HesperList | HesperListEnhanced
#'  A HesperList or HesperListEnhanced object.
#'
#' @export
from_data_frame <- S7::new_generic(
  "from_data_frame",
  dispatch_args = "df",
  function(
    df,
    hesper_vars,
    allow_missing = TRUE,
    other_vars = NULL,
    priority_vars = NULL,
    enhanced = TRUE
  ) {
    S7::S7_dispatch()
  }
)

#' @rdname from_data_frame
#' @name from_data_frame
S7::method(from_data_frame, S7::class_data.frame) <- function(
  df,
  hesper_vars,
  allow_missing = TRUE,
  other_vars = NULL,
  priority_vars = NULL,
  enhanced = TRUE
) {
  #------ Checks

  # df is a data.frame
  if (!is.data.frame(df)) {
    rlang::abort("df must be a data.frame")
  }

  # hesper_vars is a character vector of at least one column name in df
  checkmate::assert_character(hesper_vars, min.len = 1)
  checkmate::assert_subset(hesper_vars, colnames(df))

  # allow_missing is a single logical value
  checkmate::assert_logical(allow_missing, len = 1)

  # other_vars is NULL or a character vector of column names in df not in hesper_vars
  if (!is.null(other_vars)) {
    checkmate::assert_character(other_vars, min.len = 1)
    checkmate::assert_disjunct(other_vars, hesper_vars)
    checkmate::assert_subset(other_vars, setdiff(colnames(df), hesper_vars))
  }

  # priority_vars is NULL or a character vector of exactly three column names in df not in hesper_vars
  if (!is.null(priority_vars)) {
    checkmate::assert_character(priority_vars, len = 3)
    checkmate::assert_disjunct(priority_vars, hesper_vars)
    checkmate::assert_subset(priority_vars, setdiff(colnames(df), hesper_vars))
    if (!enhanced) {
      rlang::abort("priority_vars can only be used if enhanced = TRUE")
    }
  }

  # enhanced is a single logical value
  checkmate::assert_logical(enhanced, len = 1)

  #------ Prepare HesperList or HesperListEnhanced

  # get other_vars if not provided
  if (is.null(other_vars)) {
    other_vars <- setdiff(colnames(df), hesper_vars)
  }

  # prepare other_list if enhanced
  if (enhanced) {
    other_list <- df[, other_vars, drop = FALSE]
    other_list <- as.list(other_list)
  }

  # prepare hesper_list
  hesper_list <- purrr::map(hesper_vars, \(var) {
    HesperVector(
      hesper_var = var,
      hesper_vals = df[[var]],
      allow_missing = allow_missing
    )
  })

  # prepare hesper_priorities if priority_vars provided
  if (enhanced && !is.null(priority_vars)) {
    priority_list <- list(HesperPriorities(
      top1 = df[[priority_vars[1]]],
      top2 = df[[priority_vars[2]]],
      top3 = df[[priority_vars[3]]],
      allow_missing = allow_missing
    ))
  } else {
    priority_list <- list()
  }

  hl <- if (enhanced) {
    HesperListEnhanced(
      hesper_list = hesper_list,
      SL = list(),
      other_list = other_list,
      priority_list = priority_list
    )
  } else {
    HesperList(hesper_list = hesper_list)
  }

  return(hl)
}
