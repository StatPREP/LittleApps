#' Graphics functions for Little Apps
#'
#' These functions produce the graphics used in the Little Apps. There is typically
#' one function for each app. They are broken out from the application
#' so that they can be used in other places, such as notes.
#'
#' @param state a list containing the state established by the user interface
#'
#' @details The `state` argument is a list with named elements. It is the primary
#' means of communication between the app's input widgets and the graph-drawing. When
#' used as an app, the state is established in the Shiny server. But it can also
#' be constructed "by hand," so that the plotting functions can be used in other
#' contexts than the shiny app.
#'
#' Different apps may establish and use different components of the state, but there
#' are some components that many apps have in common. New plotting functions should be
#' written using the conventions  below:
#'
#' * `data`: a data frame with all of the variables from the original data being used
#' in the plot.
#' * `response`: character string naming the response variable
#' * `explan`: character string naming a single explanatory variable
#' * `covar` : character string vector naming zero or more covariates
#' * `model_formula`: an R formula object laying out the response and explanatory
#' variables as they would be used in, e.g., `lm()` or `glm()` and other model-building
#' functions. All of the variables must be available as named components of `data`.
#' * `samp_n`: an integer specifying the number of cases in the data. (This could be
#' calculated directly from the `data` component, and maybe things should be refactored that way. But meanwhile ...)
#' * `y_range`: a numeric vector of length 2 giving the lower and upper bounds of the
#' y-axis. This is provided so that the designer of the graphics can arrange to have
#' the response variable plotted on the same scale regardless of which values happen
#' to be in the sample contained in `data`. This can help to keep the y axis constant, and
#' avoid it jumping around from one sample to another.
#'
#'
#'
#'
#' @export
center_and_spread_plot <- function(state) {
  if (is.null(state$model_formula) || is.null(state$data)) return(NULL)
  set.seed(101)
  alpha <- (1 - as.numeric(state$range_level))/2
  for_quantiles <- c(alpha, 1 - alpha)
  top <- function(x) {as.numeric(quantile(x, for_quantiles[2]))}
  bottom <- function(x) {as.numeric(quantile(x, for_quantiles[1]))}
  stats <-
    df_stats(state$model_formula, data = state$data,
             stat = mean, ci = ci.mean, sd = sd,
             Q1 = bottom,
             Q2 = top,
             median = median,
             ci_med = ci.median,
             ci_sd = ci.sd,
             long_names = FALSE) %>%
    mutate(stderr = (ci_upper - stat)/2,
           sd_upper = stat + ci_sd_upper,
           sd_lower = stat + ci_sd_lower)
  P <- gf_jitter(state$model_formula, data = state$data,
                 alpha = point_alpha(state$samp_n),
                 color = state$color_formula, width = 0.1, height = 0)


  if (state$show_mean) {
    P <- P %>%
      gf_errorbar(as.formula(paste("stat + stat ~", state$explan)),
                  data = stats, color = state$color_formula,
                  width = 0.3, size = 2 )

    if (state$show_ci) {
      P <- P %>%
        gf_errorbar(as.formula(
          paste("ci_lower + ci_upper ~ ", state$explan)),
          data = stats, color = state$color_formula,
          size = 1.5, width = 0.1)
    }
  }

  if (state$show_violin) {
    P <- P %>%
      gf_violin(state$model_formula, data = state$data,
                alpha = 0.4, color = state$color_formula)
  }

  if (state$show_coverage) {
    P <- P %>%
      gf_segment(as.formula(paste("Q1 + Q2 ~ ", state$explan, "+", state$explan)),
                 data = stats, color = "blue", size = 50, alpha = 0.2) %>%
      gf_errorbar(as.formula(paste("median + median ~ ", state$explan)),
                  data = stats, color = "blue", width = 0.25)


    if (state$show_ci) {
      P <- P %>%
        gf_errorbar(as.formula(
          paste("ci_med_lower + ci_med_upper ~", state$explan)),
          data = stats,
          color = "blue", width = 0.2)
    }
  }

  if (state$show_sd) {
    P <- P %>% v_ruler(state$ruler_formula, data = stats)
    if (state$show_ci) {
      P <- P %>% gf_errorbar(as.formula(
        paste("sd_lower + sd_upper ~", state$explan)),
        data = stats,
        color = "black", width = 0.1, size = 2)
    }
  }

  P %>%
    gf_theme(legend.position = "top") %>%
    gf_lims(y = state$y_range)
}

#' Two-sample t-test
#' @export
two_sample_t_plot <-  function(state) {
  F <- state$model_formula
  COLOR <- as.formula(paste("~", state$explan))
  P <- do.call(gf_jitter, list(F, data = state$data,
                          color = COLOR,
                          width = 0.2,
                          alpha = point_alpha(state$samp_n))) %>%
    gf_theme(legend.position = "top") %>%
    gf_lims(y = state$y_range)
  this_data <- state$data
  stats <-
    if (state$ci_level == 0.95) df_stats(F, data = this_data,
                                         mn = mean,
                                         ci = ci.mean(level = 0.95))
  else df_stats(F, data = this_data,
                mn = mean,
                ci = ci.mean(level = 0.90))
  stats <- stats %>% mutate(xpos = c(1.25, 1.75))
  if (state$show_mean) {
    P <- do.call(gf_errorbar, list(P, mn + mn ~ xpos,
                                   data = stats,
                                   color = COLOR,
                                   width = 0.2,
                                   size = 2,
                                   show.legend = FALSE))
  }
  ebar_formula <- as.formula(paste("ci_lower + ci_upper ~", state$explan))

  if (state$show_ci) {
    P <- do.call(gf_errorbar, list(P, ebar_formula,
                                   data = stats,
                                   color = COLOR,
                                   width = 0.13,
                                   size = 1.5,
                                   show.legend = FALSE))
  }
  if (state$show_t) {
    tstats <- state$t_stats
    P <- P %>%
      gf_errorbar(low + high ~ midpoint, data = tstats,
                  width = 1, show.legend = FALSE)
  }
  set.seed(NULL)

  P
}
