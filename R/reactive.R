#' Create reactive-style functions that can be run outside of shiny
#'
#' Rseplacement for `shiny::reactive()`  that makes it
#' easy to shift between the reactive style in shiny and a mock-reactive style
#' that can be run in an ordinary session. In the reactive style, values used
#' in reactive functions are often produced by other reactive functions.
#'
#' @param stmts bracketed code of the same sort used in `shiny::reactive()`
#' @param env variable to set the environment in which the statements will be executed.
#' It would be rare to use the `env` variable: the default does what's needed
#' @param ... a single object (of whatever kind) to be stored in a mock-reactive variable
#'
#' @examples
#' input <- list(samp_n = 15)
#' get_data <- react_calc({head(mtcars, input$samp_n, shiny = FALSE)})
#' make_model <- react_calc({lm(mpg ~ hp, data = get_data())})
#' find_rsquared <- react_calc({summary(make_model())$r.squared})
#'
#' find_rsquared()  # the ultimate calculation
#'
#' v <- reactive_value_holder(7)
#' v() # get value
#' v(88) # set value (and return that value)
#' v() # get value
#' @export
react_calc <- function(stmts, env = parent.frame()) {
  stmts <- substitute(stmts)

  F <- function() { NULL }
  body(F) <- stmts

  F
}

#' A mock-reactive equivalent to reactiveVal

#' @export
reactive_value_holder <- function(...) {
  inputs <- list(...)
  state <- if (length(inputs) == 1) inputs[[1]]
  else NULL
  f <- function(...) {
    inputs <- list(...)
    if (length(inputs) == 1)
      state <<- inputs[[1]]

    state
  }

}


