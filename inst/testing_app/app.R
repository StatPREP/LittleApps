library(shiny)
library(LittleApps)
library(ggformula)
library(NHANES)
library(dplyr)

main_plot_function <- function(state, controls) {
  F <- as.formula(paste(state$response, "~", state$explan))
  COLOR <- as.formula(paste("~", state$explan))
  do.call(gf_jitter, list(F, data = state$data,
                          color = COLOR,
                          alpha = point_alpha(state$samp_n))) %>%
    gf_lims(y = state$y_range) %>% stats_annot_function(F, COLOR, state, controls)
}


# P **must** be first
stats_annot_function <- function(P, F, COLOR, state, controls) {
  stats <- ci_on_mean(state$data, F, state$ci_level )
  ebar_formula <- as.formula(paste("ci_lower + ci_upper ~", state$explan))
  if (state$show_ci) {
    do.call(gf_errorbar, list(P, ebar_formula,
                data = stats,
                color = COLOR,
                width = 0.13,
                size = 2,
                show.legend = FALSE))
  } else {
    P
  }
}

ci_on_mean <- function(data, formula, level = 0.95) {

  df_stats(formula, data = data,
           mn = mean, ci = ci.mean(level = !!level),
           long_names = FALSE)
}



## THE ABOVE go in the app itself. App1() will be provided by the LittleApps package.


app1 <- function(input, output) {

  # These values can be passed along to ordinary (non-reactive) functions
  V <- reactiveValues()
  observe({
    V$explan <<- req(input$explan)
    V$response <<- req(input$response)
    V$covar <<- req(input$covar)

    V$samp_n <<- as.numeric(input$samp_n)
    V$shuffle <<- shuffle_input()
    cat("Still more on V ...\n")
    V$in_each_group <<- in_each_group_input()
    V$ci_level <<- 0.95
    V$show_ci <<- show_ci_input()
    V$show_median <<- show_median_input()
    V$show_coverage <<- show_coverage_input()
    V$data <<- get_samp()
    V$y_range <<- range(get_data()[[V$response]])
  })
  shuffle_input <- reactive({
    input$shuffle
    if ("simulation" %in% names(input)) "shuffle" %in% input$simulation
    else FALSE
  })
  show_ci_input <- reactive({
    input$show_ci
    TRUE
  })
  show_median_input <- reactive({
    input$show_median
    TRUE
    })
  show_coverage_input <- reactive({
    input$show_coverage
    TRUE
    })
  in_each_group_input <- reactive({
    input$in_each_group
    FALSE
    })



  get_data <- reactive({
    cat("Input covar is", input$covar, "\n")
    res <-
      # if (input$covar == "None")
      #   NHANES[, c(input$explan, input$response, input$covar)]
      # else
        NHANES[, c(input$explan, input$response)]

    res %>% na.omit()

  })
  get_samp <- reactive( {
    cat("Getting sample\n")
    if (input$seed > 0) set.seed(as.numeric(input$seed))
    res <- get_data()
      # if (V$in_each_group) {
      #   get_data() %>% group_by_(V$explan)
      # } else {
      #   get_data()
      # }
    res <- res %>% sample_n(size = V$samp_n)

    cat("Trying again. Shuffle is", V$shuffle, "\n")
    if (V$shuffle) {
      cat("Shuffling\n")
      res[[V$explan]] <- sample(res[[V$explan]])
    }
    V$data <<- res
    res
  })

  output$plot <- renderPlot({
    # data_for_plotting <- get_samp()
    V$shuffle
    main_plot_function(V, input)
  })
  output$codebook <- renderText({
     LA_var_help("NHANES", input$response, input$explan, line_length = 40L)
   })
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  main_display(
    response_vars = get_var_names(NHANES, type = "quantitative"),
    explan_vars = get_var_names(NHANES, type = "categorical", max_levels = 4),
    covars = c("None", get_var_names(NHANES, type = "all")),
    multiple_covariates = TRUE),
  hr(),
  # App-specific controls
  fluidRow(

    column(3,
           checkboxGroupInput("layers", "Stat Annotations",
                              c("Show conf. intervals" = "show_ci",
                                "Show t interval" = "show_t",
                                "CIs @ 90% confidence level" = "at_90"))
    ),

    column(3,
           checkboxGroupInput("bogus", "t calculations"),
           htmlOutput("pvalue"),
           checkboxInput("equal_var", "Assume equal variance", FALSE),
           radioButtons("center_on", "t centered on:",
                        choices = c("Left", "Right"),
                        inline = TRUE, width = "150px")
    ),
    column(2,
           checkboxGroupInput("simulation", "Inferential Simulation",
                              c("Shuffle explanatory var." = "shuffle")))
  )


)

# Define server logic required to draw a histogram
server <- function(input, output) {


  app1(input, output)



}

# Run the application
shinyApp(ui = ui, server = server)

