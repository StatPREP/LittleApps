# Smoothing app

library(shiny)
library(LittleApps)
library(ggformula)
library(NHANES)
library(dplyr)
library(forcats)
library(mosaicCore)

# additional display tabs for this app
ANOVA <- verbatimTextOutput("anova_report")
REGRESSION <- verbatimTextOutput("regression_report")

# Define UI for application that draws a histogram
ui <- fluidPage(
  main_display(
    name = "Smoothing",
    response_vars = get_var_names(NHANES, type = "quantitative")[-1],
    explan_vars = get_var_names(NHANES, type = "quantitative"),
    covars = c(NONE = "None selected", get_var_names(NHANES, type = "categorical", max_levels = 5)),
    multiple_covariates = FALSE,
    stratify_sampling = FALSE,
    additional_panels = list(tabPanel("ANOVA", ANOVA),
                             tabPanel("Regression", REGRESSION))
    ),
  hr(),
  # App-specific controls
  fluidRow(
    column(3,
           checkboxGroupInput("layers", "Stat Annotations",
                              c("Show model values" = "show_mod_vals",
                                "Trace horizontally" = "trace_horiz",
                                "Trace vertically" = "trace_vert"))
    ),

    column(3,
           checkboxGroupInput("bogus", "Model controls"),
           checkboxInput("interaction", "Include interaction", TRUE, width = "150px"),
           selectInput("smoothing", "Smoothing/Curviness",
                                    choices = 1:6, width = "150px")
    ),
    column(2,
           checkboxGroupInput("simulation", "Inferential Simulation",
                              c("Shuffle response var." = "shuffle")))
  )
)

# Server logic
server <- function(session, input, output) {
  # The boilerplate app
  V <- reactiveValues()
  V$Raw_data <- NHANES
  # reactives for display tabs
  output$plot <- renderPlot({
    req(V$model_formula) # make sure V is initialized
    # two_sample_t_plot(V)
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

  #' General handling of data, selection of variables, etc.
  standard_littleapp_server(session, input, output, V)

  observe({
    explan_term <- if(input$smoothing > 1) {
      paste("ns(", V$explan, ",", input$smoothing, ")")
    } else {
      V$explan
    }
    covars <- V$covar[V$covar != "None selected"]

    if (length(covars) > 0) { # there are covariates in the model
      V$model_formula <<- as.formula(paste(
        V$response, "~", explan_term,
        ifelse(input$interaction, "*", "+"),
        paste(covars, collapse = " + ")
      ))
    } else
    V$model_formula <<- as.formula(paste(V$response, "~", explan_term))
  })

  observe({
    V$show_mod_vals <<-
      if ("layers" %in% names(input)) "show_mod_vals" %in% input$layers
      else FALSE
    V$trace_horiz <<-
      if ("layers" %in% names(input)) "trace_horiz" %in% input$layers
      else FALSE
    V$trace_vert <<-
      if ("layers" %in% names(input)) "trace_vert" %in% input$layers
      else FALSE
  })

}

# Run the application
shinyApp(ui = ui, server = server)

