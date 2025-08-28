#' Create binary vectors for categorical comparisons
#'
#' @typed vec_chr: character[1+]
#'  Character vector to compare against reference values
#' @typed ref_chr: character[1+]
#'  Character vector of possible values to create binaries for
#'
#' @typedreturn list
#'  Named list where each element is a binary integer vector indicating
#'  which positions in vec_chr match each reference value
#'
#' @keywords internal
create_binary_vectors <- function(vec_chr, ref_chr) {
  # ------ Checks
  checkmate::assert_character(vec_chr, min.len = 1, any.missing = TRUE)
  checkmate::assert_character(ref_chr, min.len = 1, any.missing = TRUE)

  purrr::map(ref_chr, function(ref_val) {
    as.integer(vec_chr == ref_val)
  }) |>
    purrr::set_names(ref_chr)
}
