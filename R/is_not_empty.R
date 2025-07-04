#' Check if a vector is not empty, i.e. not null, positive length and not filled with NA

#' @param x A vector
#' @return TRUE if the vector is not empty, FALSE otherwise
#'
is_not_empty <- function(x) {
  !is.null(x) & length(x) > 0 & !all(is.na(x))
}
