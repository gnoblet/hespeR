#' Get variables contained in hespeR S7 objects
#'
#' Returns the variable names contained in hespeR S7 objects, for instance @hesper_var in HesperVector or all @hesper_var s in HesperList. For HesperCategory it returns the @vars and not the @cat.
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
  function(x) {
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
