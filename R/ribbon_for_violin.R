#' Create data for a horizontal ribbon that mimics a violin plot
#'
#' gf_violin() wants the quantitative variable to be on the y axis, but
#' sometimes you want a horizontal violin on top of other plots that can't be rotated,
#' for instance if they have a density plot or histogram.
#'
#' @param var a vector of numbers: the variable to be plotted
#' @param groups a numerical vector specifying the vertical position of each group.
#' @param vpos a number specifying the y-position of the center of the violin. See details.
#' @param vwidth the maximum half-height of the violin of a box in which
#' the violin will be drawn. The actual width will be set so that the violin
#' occupies 1/4 of the box.
#' @param bw a bandwidth specified per `stats::density()`
#' @param adjust a number that can be used to adjust the bandwidth
#'
#' @details If there are multiple groups, then vpos should be a numeric vector specifying
#' the position of each group's ribbon. If it's a single number, that number will be repeated
#' for all levels of the group.
#'
#' @examples
#' # all together
#' ribbon_data <- ribbon_for_violin(mtcars$mpg, vpos = 3)
#' gf_ribbon(top + bottom ~ x, data = ribbon_data)
#' # several groups
#' ribbon_data <- ribbon_for_violin(mtcars$mpg, mtcars$cyl, vpos = 1:3)
#' gf_ribbon(top + bottom ~ x, data = ribbon_data, fill = ~ group)
#' @export
ribbon_for_violin <- function(var, groups = NULL,
                              vpos=0, vwidth = 0.4,
                              bw = "nrd0", adjust = 1) {
  box_frac <- 1/4
  box_width <- diff(range(range(var)))

  if (is.null(groups)) {
    groups <- 1
  }

  total <- NULL
  group_labels <- unique(groups)
  if (length(vpos) == 1) vpos <- rep(vpos, length(group_labels))

  if (length(group_labels) > 20)
    stop("You've got a lot of groups. Typically, the entries in groups will be a small set of integers.")

  for (k in seq_along(group_labels)){
    v <- vpos[k]
    estimate <- density(var[groups == group_labels[k]],
                        bw = bw, adjust = adjust, cut = 2, na.rm = TRUE)
    estimate$y <- box_frac * vwidth * estimate$y * box_width
    res <- data.frame(x = estimate$x,
                      top = v + estimate$y,
                      bottom = v - estimate$y,
                      group = group_labels[k])
    total <- rbind(total, res)
  }

  total$group <- as.factor(total$group)

  total
}
