#' Layout utilities for little apps
#'
#' The shiny UI collection returned by this function has `input$` handles
#' that can be used in the server logic.
#' - `plot` the target for `output$plot <- renderPlot()`
#' - `response` the `input$` component holding the name of the response variable.
#' - `explan` similarly for an explanatory variable
#' - `covar` similarly for covariates. This might be a vector of names if `multiple_covariates = TRUE`
#' - `seed` seed to generate a new sample
#' - `explain` the target for `output$explain <- renderHTML()`
#' - `codebook` the target for `output$codebook <- renderText()`

#' @param sample_sizes numerical vector giving allowable sample sizes
#' @param brush the brush options for the plot in shiny (see `shiny::brushOpts()`)
#' @param response_vars character vector giving names of response variables
#' @param explan_vars character vector giving names of response variables
#' @param covars character vector giving names of response variables
#' @param multiple_covariates Logical. If `TRUE`, allow multiple covariates to be selected.
#' @param balance_groups Logical. If `TRUE`, add a checkbox to put n in each group
#' @param ... Other shiny controls to be added to data-specification panel
#'
#'
#' @return A shiny tag that can be displayed as one entity.
#'
#' @export
main_display <- function(
  name = "Name of app",
  sample_sizes = c(5,10,20,50,100,500,1000),
  brush = brushOpts("horiz_line", direction = "y", fill = NA),
  response_vars = NULL,
  explan_vars = "None",
  covars = "None",
  multiple_covariates = FALSE,
  balance_groups = FALSE,
  ...) {

  PLOT <- plotOutput("plot", brush = brush)
  EXPLAIN <- htmlOutput("explain")
  CODEBOOK <- verbatimTextOutput("codebook")
  R_COMMANDS <- htmlOutput("code")
  DISPLAY <- tabsetPanel(tabPanel("Graphic", PLOT),
                         tabPanel("Explain", EXPLAIN),
                         tabPanel("Codebook", CODEBOOK),
                         tabPanel("R Commands", R_COMMANDS))
  fluidRow(
    column(3, # Data specification panel
           h4("Little App:"),
           h4(name), hr(),
           selectInput('response', 'Response Variable', response_vars),
           selectInput('explan', 'Explanatory Var.', explan_vars),
           selectizeInput("covar", "Covariates", covars,
                          multiple = multiple_covariates),
           selectInput("samp_n", "Sample size",
                       choices = c(5, 10, 20, 50, 100, 200, 500, 1000),
                       selected = "100"),
           if (balance_groups)
             checkboxInput("balance_groups", "Balance groups", FALSE)
           else NULL,
           hr(),
           actionButton("seed", "Take new sample"),
           ...),
    # Display tabset
    column(6,DISPLAY)
  )
}
