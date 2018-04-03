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

main_plot_function <- function(state) {
  if (is.null(state$model_formula) || is.null(state$data)) return(NULL)
  set.seed(101)
  alpha <- (1 - as.numeric(state$range_level))/2
  for_quantiles <- c(alpha, 1 - alpha)
  top <- function(x) {as.numeric(quantile(x, for_quantiles[2]))}
  bottom <- function(x) {as.numeric(quantile(x, for_quantiles[1]))}
  stats <-
    df_stats(state$model_formula, data = state$data,
             stat = mean, ci = ci.mean, sd = sd,
             Q1 = bottom,
             Q2 = top,
             median = median,
             ci_med = ci.median,
             ci_sd = ci.sd,
             long_names = FALSE) %>%
    mutate(stderr = (ci_upper - stat)/2,
           sd_upper = stat + ci_sd_upper,
           sd_lower = stat + ci_sd_lower)
  P <- gf_jitter(state$model_formula, data = state$data,
                 alpha = point_alpha(state$samp_n),
                 color = state$color_formula, width = 0.1, height = 0)


  if (state$show_mean) {
    P <- P %>%
      gf_errorbar(as.formula(paste("stat + stat ~", state$explan)),
                  data = stats, color = state$color_formula,
                  width = 0.3, size = 2 )

    if (state$show_ci) {
      P <- P %>%
        gf_errorbar(as.formula(
          paste("ci_lower + ci_upper ~ ", state$explan)),
          data = stats, color = state$color_formula,
          size = 1.5, width = 0.1)
    }
  }

  if (state$show_violin) {
    P <- P %>%
      gf_violin(state$model_formula, data = state$data,
                alpha = 0.4, color = state$color_formula)
  }

  if (state$show_coverage) {
    P <- P %>%
      gf_segment(as.formula(paste("Q1 + Q2 ~ ", state$explan, "+", state$explan)),
                 data = stats, color = "blue", size = 50, alpha = 0.2) %>%
      gf_errorbar(as.formula(paste("median + median ~ ", state$explan)),
                  data = stats, color = "blue", width = 0.25)


    if (state$show_ci) {
      P <- P %>%
        gf_errorbar(as.formula(
          paste("ci_med_lower + ci_med_upper ~", state$explan)),
          data = stats,
          color = "blue", width = 0.2)
    }
  }

  if (state$show_sd) {
    P <- P %>% v_ruler(state$ruler_formula, data = stats)
    if (state$show_ci) {
      P <- P %>% gf_errorbar(as.formula(
        paste("sd_lower + sd_upper ~", state$explan)),
        data = stats,
        color = "black", width = 0.1, size = 2)
    }
  }

  P %>%
    gf_theme(legend.position = "top") %>%
    gf_lims(y = state$y_range)
}

# The list of annotations available.
# Broken out here so that the %range label can be modified as the level changes
annot_choices <-
  c("Mean" = "show_mean",
    "Median & % range" = "show_coverage",
    "Standard deviation" = "show_sd",
    "Confidence interval" = "show_ci",
    "Density violin" = "show_violin")



# Define UI for application that draws a histogram
ui <- fluidPage(
  main_display(
    name = "Center & Spread",
    response_vars = get_var_names(NHANES, type = "quantitative"),
    explan_vars = c(get_var_names(NHANES, type = "categorical", max_levels = 2),
                    "No_explanatory_variable"),
    covars = list("None used in this app." = ""), #get_var_names(NHANES, type = "all"),
    multiple_covariates = TRUE,
    balance_groups = TRUE
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

  output$plot <- renderPlot({
    main_plot_function(V)
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
}

# Run the application
shinyApp(ui = ui, server = server)

