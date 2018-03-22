#' Add rulers to a plot
#'
#' This is still under development. In particular, I'm not sure how to pass
#' the arguments `color`, `alpha`, and so on to the gf_functions being called.
#' Currently, they are fixed. The `width` argument is particularly important.
#'
#' @examples
#' rule_data <- data.frame(center = c(3,5), length = c(1.5, 2.7), x = c("A", "B"))
#' v_ruler(center + length ~ x, data = rule_data, width = NULL)
#'
#' @export
v_ruler <- function(object = NULL, gformula = NULL, data = NULL, description = "sd", nticks = 3,
                    color = "black", alpha = 1, size = 0.5, width = NULL) {

  others <- list(color = "black", alpha = 1, size = 0.5, width = 1)
  if (inherits(object, "formula")) {
    gformula <- object
    object <- NULL
  }
  if (inherits(object, "data.frame")) {
    data <- object
    object <- NULL
  }

  # Formula should look be center + length ~ x

  left_names <- all.vars(rlang::f_lhs(gformula))
  center_var_name <- left_names[1]
  length_var_name <- left_names[2]
  x_var_name <- all.vars(rlang::f_rhs(gformula))

  # if (is.null(width)) {
  #   if (is.numeric(data[[x_var_name]])) width <- 0.5 * diff(range(data[[x_var_name]], na.rm = TRUE))
  #   else width <- 0.1 # for categorical axis
  # }

  # The relative positions of the ticks
  rel_ticks <- seq(-nticks, nticks)[-1] #drop the lowest one, it will be put back later

  # Replicate each row of the data fram so each original case will have one
  # row for each of the `rel_ticks`
  inds <- rep(1:nrow(data), each = length(rel_ticks))

  rep_data <- data[inds,]

  # Will use geom_errorbar() to draw the ruler, so we need
  # a top and bottom of each segment
  tick_number <- rep(rel_ticks, nrow(data))
  top_tick_pos <- rep_data[[center_var_name]] +
    rep_data[[length_var_name]] * tick_number
  bottom_tick_pos <- rep_data[[center_var_name]] +
    rep_data[[length_var_name]] * (tick_number - 1)
  tick_name <- paste0(as.character(tick_number), description)

  rep_data[[".top."]] <- top_tick_pos
  rep_data[[".bottom."]] <- bottom_tick_pos
  rep_data[[".name."]] <- tick_name

  interval_formula <- as.formula(paste(".top. + .bottom. ~", x_var_name))
  label_formula <- as.formula(paste(".top. ~", x_var_name))

  #return(rep_data)


  if (is.null(object)) {
    gf_errorbar(interval_formula, data = rep_data,
                size = size, color = "black", alpha = alpha, width = 0.1) %>%
      gf_text(label_formula, data = rep_data, label = ~ .name., nudge_x = -0.1)
  } else {
    object %>%
      gf_errorbar(interval_formula, data = rep_data, color = "black",
                  others, width = 0.1) %>%
      gf_text(label_formula, data = rep_data, color = "black", label = ~ .name., nudge_x = -0.1)
  }


}
