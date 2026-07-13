#' House-style colours
#'
#' Colour constants used by every plot in the package, plus
#' `ta_year_palette()`, which returns `n` distinct colours (interpolated
#' beyond the nine base colours).
#'
#' @param n Number of colours to return.
#'
#' @return `ta_year_palette()` returns a character vector of `n` hex colours;
#'   the constants are single hex strings (`ta_years_base` is a vector of
#'   nine).
#' @name ta_colors
#' @examples
#' ta_year_palette(3)
NULL

#' @rdname ta_colors
#' @export
ta_navy <- "#00365E"

#' @rdname ta_colors
#' @export
ta_blue <- "#0073AB"

#' @rdname ta_colors
#' @export
ta_lightblue <- "#A8C8E0"

#' @rdname ta_colors
#' @export
ta_gold <- "#D39F27"

#' @rdname ta_colors
#' @export
ta_muted <- "#6B7A8D"

#' @rdname ta_colors
#' @export
ta_years_base <- c("#00365E", "#0073AB", "#1A8FC2", "#4AADD4",
                   "#A8C8E0", "#D39F27", "#6B7A8D", "#C44536", "#8B5E3C")

#' @rdname ta_colors
#' @export
ta_year_palette <- function(n) {
  if (n <= length(ta_years_base)) ta_years_base[1:n]
  else colorRampPalette(ta_years_base)(n)
}
