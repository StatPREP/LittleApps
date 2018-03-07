#' Select one of the available apps and run it.
#'
#' For convenience only, this function will display a list of the available apps and let you run
#' one.
#'
#' @details In actual use, you would display the app in a learnr tutorial or another Rmd document containing
#' narrative describing the app.
#'
#' @export
select_app <- function() {
  possibilities <- dir(system.file(package = "StatPREPshiny"), pattern = ".Rmd$")
  which_app <- menu(possibilities)
  if (which_app > 0) run(file = system.file(possibilities[which_app], package = "StatPREPshiny"))

  NULL
}
