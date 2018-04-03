#' Implementing LittleApps color policies
#'
#' @param component character string naming the component of the graphic
#' @param covar_name character string naming the covariate
#'
#' Color should be used in a consistent way in the LittleApps. The `LA_colors()` function refers to
#' this policy. Using `LA_colors()` in the LittleApps enables the policy to be changed while maintaining
#' consistence across apps.
#'
#' sample - the points in the sample used in the primary display: black or a color set by a covariate
#'
#' resample_trial - orange
#' resample_vals - data points in the resample
#' sample_trial - model values for a new trial of sampling (like resample, but from the population)
#' sample_sim - data points in the sampling trial
#' response_annot - values for the response variable shown as an annotation
#' model_val_annot - values for the model values shown as an annotation
#' model_fun - depiction of the function modeling the relationship shown,
#' e.g. group means, a regression function: black or a color set by a covariate
#' conf_int - error bars used to display confidence intervals: blue
#'
#' @examples
#' # What color for a confidence interval?
#' LA_color("conf_int")
#' # Colors for groupwise means when "gender" is a covariate
#' LA_color("model_fun", "gender")
#' # Or when the user has selected no covariate, flag is "none" or "no covar"
#' selected_covar <- "none"
#' LA_color("model_fun", selected_covar)
#'
#' @export
LA_color <- function(component = c("sample", "resample_vals", "resample_trial", "sample_trial", "sample_sim", "response_annot", "model_val_annot",
                                    "model_fun", "conf_int"),
                      covar_name = NULL) {
  # Equivalent to no covariate: NULL or ...
  if ( !is.null(covar_name) && tolower(covar_name) %in% c("", " ", "none", "no covar"))
    covar_name <- NULL
  component <- match.arg(component)
  switch(component,
         sample = if (is.null(covar_name)) "black" else
           as.formula(paste("~", covar_name)),
         resample_trial = "orange",
         resample_vals = "orange",
         sample_trial = "green",
         sample_sim = "green", # in
         response_annot = "red",
         model_val_annot = "red",
         model_fun = if (is.null(covar_name)) "black" else
           as.formula(paste("~", covar_name)),
         conf_int = "blue"
  )
}

#' Set transparency appropriately for a scatter plot
#'
#' The transparency (alpha) value should be decreased slightly as
#' a greater number of points is displayed
#'
#' @param n the number of points to be displayed

#' @export
point_alpha <- function(n) {
  pmin(1, 0.2 + 1.6 / 2^log10(n)) # a trick?
}




#' Color scale for named colors
#' Use this so that named colors get shown as the color itself
#'
#' @examples
#' scale_y_continuous( name = "probabability", breaks = seq(0, 1, by = 0.2),
#'                     verbatim_colors())
#'
#' @export
verbatim_colors <- function() {
  # add more as needed
  colors = c("red", "orange", "yellow", "green", "blue", "indigo", "violet", "black", "gray")
  names(colors) <- colors
  scale_color_manual(values = colors)
}


