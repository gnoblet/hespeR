#' Functions to extract colors as hex codes
#'
#' [color()] returns the requested columns, returns NA if absent. [color_pattern()] returns all colors that start with the pattern.
#'
#' @param ... Character names of colors. If NULL returns all colors.
#' @param unname Boolean. Should the output vector be unnamed? Default to `TRUE`.
#' @section Naming of colors:
#' * All branding colors start with "branding";
#' * All categorical colors start with "cat_";
#' * All continuous colors start with "quant_";
#'
#' Then, a number indicates the number of colors that belong to the palettes, a string the name of the palette, and, finally, a number the position of the color. E.g., "quant_5_red_4" would be the 4th color of a continuous palettes of 5 colors in the red band. Exception is made for white, light_grey, dark_grey, and black.
#'
#'
#' @return Hex codes named or unnamed.
#'
#' @export
color <- function(..., unname = TRUE) {

  cols <- c(...)

  colors <- c(
      white = "#FFFFFF"
    , cat_main_1 = "#083d77" # yale blue
    , cat_main_2 = "#4ecdc4" # robin egg blue
    , cat_main_3 = "#f4c095" # peach
    , cat_main_4 = "#b47eb3" # african violet
    , cat_main_5 = "#FFD5FF" # mimi pink
    , quant_5_main_1 = "#083D77" # yale blue
    , quant_5_main_2 = "#396694"
    , quant_5_main_3 = "#6A8EB2" 
    , quant_5_main_4 = "#9BB7CF"
    , quant_5_main_5 = "#CCDFEC"
   )

  if (is.null(cols)) {
    cols_to_return <- colors
  } else {
    cols_to_return <- colors[cols]
  }

  if (unname) {
    cols_to_return <- unname(cols_to_return)
  }

  return(cols_to_return)
}


#' @rdname color
#'
#' @param pattern Pattern of the start of colors' name.
#'
#' @export
color_pattern <- function(pattern, unname = TRUE){

  col <- color(unname = FALSE)

  col <- col[startsWith(names(col), pattern)]

  if (unname) {
    col <- unname(col)
  }

  return(col)
}


#' @title Interpolate a color palette
#'
#' @param palette Character name of a palette in palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param show_palettes Should the ouput be the set of palettes names to pick from? Default to `FALSE`
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return A color palette
#'
#' @export
palette <- function(palette = "branding_reach", reverse = FALSE, show_palettes = FALSE, ...) {

  pals <- list(
    cat_main = color_pattern("cat_main")
    , quant_5_main = color_pattern("quant_5_main")
  )

  if (show_palettes) {
    return(glue::glue("Available palettes: {glue::glue_collapse(names(pals), sep = ', ')}"))
  }

  if (length(palette) != 1) rlang::abort("'palette' can only be of length 1.")

  if (palette %notin% names(pals)) rlang::abort("Please provide an existing name of palette. To show all names, use 'show_palettes = TRUE'.")

  pal <- pals[[palette]]

  if (reverse) pal <- rev(pal)

  return(pal)
}


#' Generate color palettes
#'
#' [palette_gen()] generates a color palette and let you choose whether continuous or discrete. [palette_gen_discrete()] and [palette_gen_continuous()] generates respectively discrete and continuous palettes.
#'
#' @param palette Palette name from [palette()].
#' @param type "discrete" or "continuous".
#' @param direction 1 or -1; should the order of colors be reversed?
#' @param ... Additional arguments to pass to [colorRampPalette()] when type is "continuous".
#'
#' @export
palette_gen <- function(palette, type, direction = 1, ...){

  if (type %notin% c("discrete", "continuous")) rlang::abort("'type' must be discrete or continuous.")

  if (type == "discrete") return(palette_gen_discrete(palette = palette, direction = direction))

  if (type == "continuous") return(palette_gen_continuous(palette = palette, direction = direction, ...))
}


#' @rdname palette_gen
#'
#' @export
palette_gen_discrete <- function(palette = "main", direction = 1) {

  if (abs(direction) != 1) rlang::abort("Direction must be either 1 or -1.")

  pal <- palette(palette)

  f <- function(n) {
    if (is.null(n))  n <- length(pal)

    if (n > length(pal)) rlang::warn("Not enough colors in this palette!")

    pal <- if (direction == 1) pal else rev(pal)

    pal <- pal[1:n]

    return(pal)
  }

  return(f)
}

#' @rdname palette_gen
#'
#' @export
palette_gen_continuous <- function(palette = "quant_7_artichoke", direction = 1, ...) {

  if (abs(direction) != 1) rlang::abort("Direction must be either 1 or -1.")

  pal <- palette(palette)

  pal <- if (direction == 1) pal else rev(pal)

  grDevices::colorRampPalette(pal, ...)

}