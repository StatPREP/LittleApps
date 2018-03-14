#' Color scale for categorical values
#'
#' Generates a color scale for categorical values.
#'
#' @param levels vector of character strings with names of data levels
#' @param annots vector of character strings with names of "annotation" levels
#' @param annot_colors name or names of colors to use for the annotations. Default: `"red"`. These will be
#' used in the same order as `annots` and repeated as needed.
#'
#' @export
StatPREP_color_scale <- function(levels, annots = NULL, annot_colors = "red") {
  palette <-
    if (length(levels) <= 8) RColorBrewer::brewer.pal(n = length(levels), name ="Set2")
    else if (length(levels) <= 12) RColorBrewer::brewer.pal(n = length(levels), name = "Set3")
    else rainbow(length(levels))

  names(palette) <- levels

  if ( ! is.null(annots)) {
    more_colors <- rgb(t(col2rgb(rep(annot_colors, length.out = length(annots)))/255))
    names(more_colors) <- annots
    palette <- c(palette, more_colors)
  }

  palette
}
