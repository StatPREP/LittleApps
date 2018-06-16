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
                 color = state$color_formula, width = 0.1, height = 0,
                 seed = 12345)


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
                          seed = 12345,
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

#' @export
smoother_plot <- function(state) {
  color_formula <- function() {
    if (length(state$covar) == 0 | any(state$covar == "None selected")) "black"
    else as.formula(paste("~", state$covar))
  }
  # Get the model functions for plotting
  mod <- lm(state$model_formula, data = state$data)
  mfundata <- mosaicModel::mod_eval(mod, nlevels = Inf)

  mod_vals <- mod_eval(mod, data = state$data)
  x_range <- range(mfundata[[state$explan]])
  ebar_width <- diff(x_range) / 15
  x_max <- x_range[2] + 0.25 * diff(x_range)
  x_mod_vals <- x_range[2] + 0.1 * diff(x_range)
  x_raw_vals <- x_range[2] + 0.2 * diff(x_range)
  mod_vals$x_mod_vals <- x_mod_vals
  mod_vals$x_raw_vals <- x_raw_vals
  mod_vals$raw <- state$data[[state$response]]
  ebar_width <- diff(x_range)/15
  std_bars <- data.frame(base = rep(mean(mod_vals$raw, na.rm = TRUE), 2),
                         x = c(x_mod_vals, x_raw_vals))
  std_bars$top <- std_bars$base + c(sd(mod_vals$model_output, na.rm = TRUE),
                                    sd(mod_vals$raw, na.rm = TRUE))
  frame_formula <- as.formula(paste(state$response, "~", state$explan))
  P <-
    gf_point(frame_formula, data = state$data, color = color_formula()) %>%
    gf_line(as.formula(paste("model_output ~ ", state$explan)),
            data = mfundata, color = color_formula()) %>%
    gf_theme(legend.position = "top")

  P <- P %>%
    gf_errorbar(model_output + model_output ~ x_mod_vals, data = mod_vals,
                width = 1*ebar_width, color = "red", alpha = 0.25) %>%
    gf_errorbar(raw + raw ~ x_raw_vals, data = mod_vals,
                width = 1*ebar_width, color = "red", alpha = 0.25)
  P <- P %>%
    gf_errorbar(base + top ~ x, data = std_bars,
                        width = ebar_width/2, size = 2, color = "black", alpha = 1.0)

  # # confidence bands
  # # gf_ribbon() not working for some unknown reason.
  #
  # P <- P %>% gf_ribbon(data = mfundata,
  #               as.formula(paste("lower + upper ~ ", V$explan)),
  #               alpha = 0.2, fill = color_formula(), color = NA)
  # Make sure to define INTERVALS above and to place it in user interface


  if (state$show_mod_vals)
    P <- P %>% gf_point(as.formula(paste("model_output ~", state$explan)),
                        data = mod_vals, color = color_formula(),
                        shape = 2)

  if (state$trace_horiz)
    P <- P %>% gf_segment(
      as.formula(paste("model_output + model_output ~ ", state$explan, "+ x_mod_vals")),
      data = mod_vals,
      color = "black", size = 0.1)

  if (state$trace_vert)
    P <- P %>% gf_segment(as.formula(paste("model_output + raw ~ ",
                                           state$explan, "+", state$explan)),
                          data = mod_vals,
                          color = "black", size = 0.1)



  P
}

#' @export
proportions_plot <- function(state) {
  discrete_explanatory <- state$explan == "No_explanatory_variable" ||
    length(unique(state$data[[state$explan]])) < 10
  jitter_width <- ifelse(discrete_explanatory, 0.2, 0.0)
  model_formula <-
    if (state$explan == "No_explanatory_variable") {
      binary ~ 1
    } else if (discrete_explanatory) {
      as.formula(paste0("binary ~", state$explan))
    } else {
      as.formula(paste0("binary ~ ns(", state$explan, ", ", state$smoothing, ")"))
    }
  data_formula <-
    if (state$explan == "No_explanatory_variable") {
      as.formula(paste(state$response, "~ 1"))
    } else {
      as.formula(paste0(state$response, "~", state$explan))
    }
  the_data <- state$data
  second_val <- c(sort(unique(the_data[[state$response]])))[2]
  the_data$binary <- the_data[[state$response]] == second_val
  P <- gf_jitter(data_formula, data = the_data,
                 height = 0.2, width = as.numeric(jitter_width),
                 seed = 12345,
                 alpha = point_alpha(state$samp_n)) %>%
       gf_labs(y = state$response, x = state$explan )
  # fit and evaluate model
  mod <- if (state$logistic)
    glm(model_formula, data = the_data, family = binomial)
  else lm(model_formula, data = the_data)
  model_vals <- mosaicModel::mod_eval(mod, nlevels = Inf,
                                      interval = "confidence")
  model_vals$upper <- ifelse(model_vals$upper > 1.1, 1.1, model_vals$upper)
  model_vals$lower <- ifelse(model_vals$lower < -0.1, -0.1, model_vals$lower)
  model_vals$explan <- model_vals[[state$explan]]
  # put model values on the same scale as the response variable
  model_vals <- model_vals %>%
    mutate(upper = upper + 1,
           lower = lower + 1,
           model_output = model_output + 1)

  if (state$show_ci) {
    P <- P %>% gf_errorbar(lower + upper ~ explan,
                           data = model_vals, color = LA_color("conf_int"),
                           alpha = ifelse(discrete_explanatory, 0.5, 0.1))
  }

  if (discrete_explanatory) {
    P <- P %>% gf_errorbar(model_output + model_output ~ explan,
                           data = model_vals)
  } else {
    P <- P %>% gf_line(model_output ~ explan,
                       data = model_vals, size = 2, color = LA_color("model_fun"), alpha = 0.5)
  }

  return(P)
  second_axis = dup_axis(name = "Numeric",
                         breaks = seq(0, 1, by = 0.2),
                         labels = c(0, "0.2", "0.4", "0.6", "0.8", 1))
  gf_refine(scale_y_continuous(
    name = state$response,
    breaks = seq(0, 1, by = 0.2),
    limits = state$y_lims,
    labels = c(level_labs[1], "", "", "", "", level_labs[2]),
    sec.axis = second_axis,
    verbatim_colors())) %>%
  gf_theme(legend.position = "none")
}


