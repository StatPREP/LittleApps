# Little App: Proportions

library(shiny)
library(LittleApps)
library(ggformula)
library(NHANES)
library(dplyr)
library(forcats)
library(mosaicCore)

NHANES <- NHANES %>%
  mutate(No_explanatory_variable = " ",
         "BP_over_130" = ifelse(BPSys1 >= 130, "yes", "< 130"),
         "Own_Home" = ifelse(HomeOwn == "Own", "yes", "no"),
         "Below_Poverty_Level" = ifelse(Poverty <= 1, "poor", "not"),
         "Pregnant" = ifelse(PregnantNow == "Yes", "yes", "not"),
         "Depressed" = ifelse(Depressed != "none", "yes", "no")
         )
response_vars <- c("BP_over_130", "Own_Home", "Below_Poverty_Level",
                   "Pregnant", "Depressed")

categorical_vars <- c("SmokeNow", "Depressed", "Work", "PhysActive",
                      "HomeOwn",  "PregnantNow" )
# Gender is added at the selectInput stage, so it will be second.
quantitative_vars <- c("Age", "Poverty", "Pulse", "BMI", "TotChol",
                       "BPSys1", "UrineVol1", "Weight", "Height", "DaysMentHlthBad",
                       "LittleInterest", "Age1stBaby", "PhysActiveDays", "SexNumPartnLife")


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
    name = "Proportions",
    response_vars = response_vars,
    # should add "No_explanatory_variable" to the explan_vars, but needs to be debugged
    explan_vars = c(categorical_vars, quantitative_vars),
    covars = list("None used in this app." = "None selected"), ,
    multiple_covariates = FALSE,
    stratify_sampling = FALSE
    ),
  hr(),
  # App-specific controls
  fluidRow(
    # Simulation Inference
    column(3,
           h4("Simulation inference"),
           radioButtons("sample_or_resample", "Type of simulation",
                        choices = c("Sample from population"="sample",
                                    "Resample" = "resample"),
                        selected = "sample" ),
           actionButton("new_trial", "Run new simulation trial"),
           checkboxInput("retain", "Retain trials"),
           actionButton("clear_trials", "Clear trials"),
           checkboxInput("shuffle", "Shuffle response variable")
    ),
    # Classical inference
    column(3,
           h4("Classical inference"),
           checkboxInput("show_ci", "Show 95% confidence interval")
           ),
    # Modeling
    column(3,
           h4("Modeling"),
           checkboxInput("logistic", "Use logistic regression"),
           selectInput("smoothing", "Smoothing/Curviness",
                       choices = c("NA" = 0,
                                   "linear - 1" = 1, 2:6))
    )
  )
)

# Use boilerplate server
server <- function(session, input, output) {
  # The boilerplate app
  V <- reactiveValues()
  V$Raw_data <- NHANES
  # State components for simulation
  V$seeds <- NULL
  V$sim_type <- NULL

  # Reactives for filling in the display tabs in the app
  output$plot <- renderPlot({
    proportions_plot(V)
  })
  output$codebook <- renderText({
    req(input$response)
    LA_var_help("NHANES",
                V$response, V$explan,
                line_length = 40L)
  })
  #' There should be files `code.md` and `explain.md` in the directory. These
  #' contain the materials to be displayed in the "code" and "explain" tabs.
  output$explain <- renderText({HTML(includeMarkdown("explain.md"))})
  output$code <- renderText({HTML(includeMarkdown("code.md"))})

  #' Handles creation of the data component of the state, selection of
  #' variables, and other tasks that are common across many apps
  standard_littleapp_server(session, input, output, V)


  # Update the simulation info. Keep just the random seed and trial type:
  # let the graphics function do everything else
  observe({
    if (input$new_trial > 0) {
      cat("adding new simulation\n")
      new_seed <- round(runif(1, max = 1000000, min = 1))
      new_type <- isolate(input$sample_or_resample)
      if (isolate(input$retain)) {
        V$seeds <<- c(isolate(V$seeds), new_seed)
        V$sim_type <<- c(isolate(V$sim_type), new_type)
      } else {
        V$seeds <<- new_seed
        V$sim_type <<- new_type
      }
    }
    cat("Now", length(V$seeds), "simulations\n")
  })
  observe({
    cat("clearing simulations\n")
    input$clear_trial
    V$seeds <<- V$sim_type <<- NULL
  })

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
    V$show_ci <<- input$show_ci
    V$shuffle <<- input$shuffle
    V$smoothing <<- as.numeric(input$smoothing)
    V$logistic <<- input$logistic
  })


  ylims <- reactive({
    c(-0.1, 1.1)
  })

  observe({
    V$model_formula <<- as.formula(paste(input$response, "~ ", input$explan))
    V$color_formula <<- as.formula(paste("~", input$explan))
    V$ruler_formula <<- as.formula(paste("stat + sd ~ ", input$explan))
  })


}

# Run the application
shinyApp(ui = ui, server = server)

