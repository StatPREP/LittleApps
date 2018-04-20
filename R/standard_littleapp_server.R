#' Standard server function for a Little App
#'
#' This function provides the standard set of services for a Little App
#'
#'
#' @details The argument `V` is a reactive value created *outside* of this function.
#' It holds the state which is the main means of communication among the controls
#' and the output displays.

#'
#'
#' @param session the session argument from the usual app `server()` function
#' @param input the input argument from the usual app `server()` function.
#' @param output the output argument from the usual app `server()` function
#' @param V the reactive value holding the state
#'
#'
#'
#' @examples
#' \dontrun{
#' # Your app's server function will look like this
#' server <- function(session, input, output) {
#'   V <- reactiveValues() # shared state
#'   standard_littleapp_server(session, input, output)
#'   # you can add other reactives, e.g.
#'   custom_1 <- reactive({
#'     # internal logic
#'   })
#'   output$custom2 <- renderImage({
#'     # internal logic
#'   })
#' }
#'
#' }
#'
#' @export
standard_littleapp_server <- function(session, input, output, V) {
  get_data <- reactive({
    if (any(input$covar == "None selected"))
      res <- V$Raw_data[, c(input$explan, input$response)]
    else
      res <- V$Raw_data[, c(input$explan, input$response, input$covar)]
    res %>% na.omit()
  })
  get_samp <- reactive( {
    if (input$seed > 0) set.seed(as.numeric(input$seed))
    res <- if (V$stratify_sampling) {
      get_data() %>% group_by_(V$explan)
    } else {
      get_data()
    }
    if (V$explan == V$response) {
      # dplyr::sample_n doesn't work if two columns have the same name
      res <- res[sample(1:nrow(res), V$samp_n), ]
    } else {
      res <- res %>% sample_n(size = V$samp_n)
    }
    if (V$shuffle) {
      res[[V$response]] <- sample(res[[V$response]])
    }
    V$data <<- res

    res
  })

  # These values can be passed along to ordinary (non-reactive) functions

  observe({
    V$explan <<- req(input$explan)
    V$response <<- req(input$response)
    V$covar <<- input$covar # no req() here

    V$samp_n <<- as.numeric(input$samp_n)
    V$shuffle <<- shuffle_input()
    V$stratify_sampling <<- stratify_sampling_val()

    V$data <<- get_samp()

    V$y_range <<- range(get_data()[[input$response]])
  })
  shuffle_input <- reactive({
    input$shuffle
    if ("simulation" %in% names(input)) "shuffle" %in% input$simulation
    else FALSE
  })


  stratify_sampling_val <- reactive({
    res <- if ("stratify" %in% names(input)) {
      input$stratify
    } else FALSE

    return(res)

  })



}
