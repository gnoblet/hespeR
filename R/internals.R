# not in
`%notin%`<- function(a, b) {
  !(a %in% b)
}


# infix for null replacement
#' @importFrom rlang `%||%`
`%ifnullrep%` <- function(a, b) {
  a %||% b
}