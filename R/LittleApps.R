#' Support for the LittleApps sequence of shiny apps
#'
#' In order to include Shiny apps in a learnr tutorial, the apps
#' have been written as Rmd documents. These can then be included in a tutorial
#' by using the `child = system.file("name_of_app.Rmd", package = "LittleApps")`
#' option in a chunk.
#'
#' @details In the app Rmd files, the chunks containing the server part of
#' the app should be marked as `context = "server"`.
#'
#' @name LittleApps
NULL
