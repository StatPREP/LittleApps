#' Utility for identifying variables of different sorts
#'
#' Returns the names of variables in `data` that meet certain criteria:
#'
#' type "quantitative" -- any variable that's numeric
#' type "categorical" -- any variable thats a factor, a character, or a logical. Must have no nore than
#' `max_levels` levels.
#' type "discrete" -- either quantitative or categorical, having no more than `max_levels` distinct values
#' type "all" -- anything with enough non-missing values
#'
#' @param data a data table
#' @param type quantitative or categorical
#' @param max_levels integer specifying how many levels categorical variables
#' @param at_least_n minimum number of not-missing values in the variable
#' can have to be included in the list
#'
#' @examples
#' get_var_names(mtcars, type = "discrete", max_levels = 3)
#'
#' @export
get_var_names <- function(data,
                          type = c("quantitative", "categorical", "discrete", "all"),
                          max_levels = 10L,
                          at_least_n = 0L) {
  type <- match.arg(type) # make sure it's in the accepted list

  if (type == "all") return(names(data))

  accept_fun <- function(x) {

    # must have enough non-missing values
    n_not_missing <- sum(!is.na(x))
    if (n_not_missing < at_least_n) return(FALSE)

    if (type == "all") return(TRUE)

    if (is.numeric(x) && type == "quantitative") return(TRUE)

    # Rule out any with too many levels
    nlevels <- sum( ! is.na(unique(x))) # NA doesn't count
    if (nlevels > max_levels) return(FALSE)

    if (type == "categorical" ) {
      if(is.factor(x) || mode(x) %in% c("character", "logical")) return(TRUE)
    }

    if (type == "discrete") return(TRUE)

    FALSE
  }

  keepers <- unlist(lapply(data, FUN = accept_fun))

  names(data)[keepers]
}

