#' Get variables contained in hespeR S7 objects
#'
#' Returns the variable names contained in hespeR S7 objects, for instance @hesper_var in HesperVector or all @hesper_var s in HesperList. For HesperCategory it returns the @vars and not the @cat. For HesperListEnhanced objects, default uses get_vars on the @hesper_list slot.
#'
#' @typed x: HesperVector | HesperList | HesperCategories | SL
#'  An S7 object of class HesperVector, HesperList, HesperCategory, HesperCategories or SL.
#' @typedreturn character[0+]
#'  A character vector of variable names.
#'
#' @export
get_vars <- S7::new_generic(
  "get_vars",
  dispatch_args = "x",
  function(x, ...) {
    S7::S7_dispatch()
  }
)

#' @rdname get_vars
#' @name get_vars
#'
S7::method(get_vars, HesperVector) <- function(x) {
  return(x@hesper_var)
}

#' @rdname get_vars
#' @name get_vars
#'
S7::method(get_vars, HesperList) <- function(x) {
  hvs <- purrr::map_chr(x@hesper_list, \(x) get_vars(x))
  return(hvs)
}

#' @rdname get_vars
#' @name get_vars
#'
S7::method(get_vars, HesperCategory) <- function(x) {
  return(x@vars)
}

#' @rdname get_vars
#' @name get_vars
#'
S7::method(get_vars, HesperCategories) <- function(x) {
  vars <- purrr::map(x@category_list, \(x) get_vars(x))
  vars <- unlist(vars)
  return(vars)
}

#' @rdname get_vars
#' @name get_vars
#'
S7::method(get_vars, SL) <- function(x) {
  return(x@hesper_var)
}

#' @rdname get_vars
#' @name get_vars
#'
#' @typed property: character[1]
#'
#'
S7::method(get_vars, HesperListEnhanced) <- function(
  x,
  property = 'hesper_list'
) {
  #------ Checks

  checkmate::assert_choice(
    property,
    choices = c(
      'hesper_list',
      'hesper_list_pp',
      "category_list",
      "category_list_pp",
      'SL',
      'other_list'
    )
  )

  #------ Get vars

  vars <- if (property %in% c('hesper_list', 'hesper_list_pp')) {
    p <- S7::prop(x, property)
    purrr::map_chr(p, \(x) x@hesper_var)
  } else if (property %in% c('category_list')) {
    if (length(x@category_list) == 0) {
      return(character(0))
    }
    cats <- purrr::map(S7::prop(x, property), \(x) get_vars(x))
    cats <- unlist(cats)
    cats
  } else if (property %in% c('category_list_pp')) {
    if (length(x@category_list_pp) == 0) {
      return(character(0))
    }
    x@category_list_pp |> names()
  } else if (property == 'SL') {
    if (length(x@SL) == 0) {
      return(character(0))
    }
    purrr::map_chr(x@SL, \(x) x@hesper_var)
  } else if (property == 'other_list') {
    if (length(x@other_list) == 0) {
      return(character(0))
    }
    names(x@other_list)
  }

  return(vars)
}
