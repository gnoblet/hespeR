#' Get property or subset from HesperListEnhanced objects
#'
#' This method provides flexible access to properties within HesperListEnhanced objects.
#' You can retrieve entire properties or specific subsets based on indices, names, or conditions.
#'
#' @typed x: HesperListEnhanced
#'   A HesperListEnhanced S7 object.
#' @typed property: character[1]
#'   The property name to retrieve either 'hesper_list', 'hesper_list_pp' or 'SL'.
#' @typed hesper_vars: character[0+] | NULL
#'  Item names identifiers to filter the property.
#'
#' @typedreturn list
#'   Returns a list containing the requested property (subsetted if specified).
#'
#' @export
get <- S7::new_generic(
  "get",
  dispatch_args = "x",
  function(x, property, hesper_vars = NULL) {
    S7::S7_dispatch()
  }
)

#' @rdname get
#' @name get
S7::method(get, HesperListEnhanced) <- function(
  x,
  property,
  hesper_vars = NULL
) {
  # Validation
  checkmate::assert_choice(
    property,
    choices = c(
      'hesper_list',
      'hesper_list_pp',
      'SL'
    )
  )
  if (property %in% c('hesper_list', 'hesper_list_pp', 'SL')) {
    checkmate::assert_character(hesper_vars, null.ok = TRUE, min.len = 0)
    if (!is.null(hesper_vars) && !all(hesper_vars %in% get_vars(x))) {
      hv_nin <- hesper_vars[!hesper_vars %in% get_vars(x)]
      rlang::abort(c(
        glue::glue(
          "Some or all provided `hesper_vars` not found in property @{property}."
        ),
        "i" = glue::glue(
          "The following `hesper_vars` are not found: ",
          glue::glue_collapse(hv_nin, sep = ", ")
        ),
        "*" = "Check `get_vars()` to see available vars."
      ))
    }
  }

  # Get the property
  p <- S7::prop(x, property)

  # Subset if hesper_vars provided
  if (!is.null(hesper_vars)) {
    p <- p[which(get_vars(x, property) %in% hesper_vars)]
  }

  return(p)
}
