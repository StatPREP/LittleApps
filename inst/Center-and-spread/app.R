# Little App: CENTER and SPREAD

library(shiny)
library(LittleApps)
library(ggformula)
library(NHANES)
library(dplyr)
library(forcats)
library(mosaicCore)

NHANES <- NHANES %>%
  mutate(idealized_normal  = rnorm(nrow(NHANES)),
         idealized_uniform = runif(nrow(NHANES)),
         No_explanatory_variable = " ")


# The list of annotations available.
# Broken out here so that the %range label can be modified as the level changes
annot_choices <-
  c("Mean" = "show_mean",
    "Median & % range" = "show_coverage",
    "Standard deviation" = "show_sd",
    "Confidence interval" = "show_ci",
    "Density violin" = "show_violin")



# Define UI
ui <- fluidPage(
  main_display(
    name = "Center & Spread",
    response_vars = get_var_names(NHANES, type = "quantitative"),
    explan_vars = c(get_var_names(NHANES, type = "categorical", max_levels = 2),
                    "No_explanatory_variable"),
    covars = list("None used in this app." = ""), #get_var_names(NHANES, type = "all"),
    multiple_covariates = TRUE,
    stratify_sampling = FALSE
    ),
  hr(),
  # App-specific controls
  fluidRow(
    column(3,
           checkboxGroupInput("layers", "Stat Annotations",
                              choiceNames = names(annot_choices),
                              choiceValues = as.character(annot_choices))
    ),

    column(2,
           selectInput("range_level", "Range level",
                       choices = c("99%" = 0.99, "95%" = 0.95,
                                   "80%" = 0.80, "67%" = 0.67, "50%" = 0.50),
                       selected = 0.95),
           checkboxGroupInput("simulation", "Inferential Simulation",
                              c("Shuffle explanatory var." = "shuffle")))
  )
)

# Use boilerplate server
server <- function(session, input, output) {
  # The boilerplate app
  V <- reactiveValues()
  V$Raw_data <- NHANES

  # Reactives for filling in the display tabs in the app
  output$plot <- renderPlot({
    center_and_spread_plot(V)
  })
  output$codebook <- renderText({
    req(input$response)
    LA_var_help("NHANES",
                V$response, V$explan, V$covar,
                line_length = 40L)
  })
  #' There should be files `code.md` and `explain.md` in the directory. These
  #' contain the materials to be displayed in the "code" and "explain" tabs.
  output$explain <- renderText({HTML(includeMarkdown("explain.md"))})
  output$code <- renderText({HTML(includeMarkdown("code.md"))})

  #' Handles creation of the data component of the state, selection of
  #' variables, and other tasks that are common across many apps
  standard_littleapp_server(session, input, output, V)



  observe({
    # These uses of V are specific to this app.
    V$model_formula <<- as.formula(paste(input$response, "~", input$explan))
  })

  # Refresh label on the XX% range
  observe({
    names(annot_choices)[2] <- paste0("Median and ",
                                round(100*as.numeric(req(input$range_level))),
                                "% range.")
    vals <- isolate(input$layers)
    updateCheckboxGroupInput(session, "layers",
                        choices = annot_choices,
                        selected = vals)
  })


  observe({
    V$show_sd <<-
      if ("layers" %in% names(input)) "show_sd" %in% input$layers
      else FALSE
    V$show_mean <<-
      if ("layers" %in% names(input)) "show_mean" %in% input$layers
      else FALSE
    V$show_ci <<-
      if ("layers" %in% names(input)) "show_ci" %in% input$layers
      else FALSE
    V$show_t <<-
      if ("layers" %in% names(input)) "show_t" %in% input$layers
      else FALSE
    V$show_median <<-
      if ("layers" %in% names(input)) "show_median" %in% input$layers
      else FALSE
    V$show_violin <<-
      if ("layers" %in% names(input)) "show_violin" %in% input$layers
      else FALSE
    V$show_coverage <<-
      if ("layers" %in% names(input)) "show_coverage" %in% input$layers
      else FALSE
    V$range_level <<- input$range_level
  })


  ylims <- reactive({
    lims <- range(the_data()[[response()]])
    lims + c(-0.2, 0.2) * diff(lims)
  })

  observe({
    V$model_formula <<- as.formula(paste(input$response, "~ ", input$explan))
    V$color_formula <<- as.formula(paste("~", input$explan))
    V$ruler_formula <<- as.formula(paste("stat + sd ~ ", input$explan))
  })


}

# Run the application
shinyApp(ui = ui, server = server)

