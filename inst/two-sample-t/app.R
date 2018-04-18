# Center and spread app

library(shiny)
library(LittleApps)
library(ggformula)
library(NHANES)
library(dplyr)
library(forcats)
library(mosaicCore)

# Dicotomize some variables of particular interest
NHANES <- NHANES %>%
  mutate(PregnantNow = fct_recode(PregnantNow, NULL = "Unknown"),
         HomeOwn     = fct_recode(HomeOwn,    NULL = "Other" ),
         Depressed   = fct_recode(Depressed,   Several = "Most"),
         Work        = fct_recode(Work, NotWorking = "Looking"),
         LittleInterest = fct_recode(LittleInterest, Several = "Most"))






# Define UI for application that draws a histogram
ui <- fluidPage(
  main_display(
    name = "Two-sample t",
    response_vars = get_var_names(NHANES, type = "quantitative"),
    explan_vars = get_var_names(NHANES, type = "categorical", max_levels = 2),
    covars = list("Not relevant to t statistic." = ""), #get_var_names(NHANES, type = "all"),
    multiple_covariates = TRUE,
    stratify_sampling = FALSE
    I'm trying to get this to work when stratify_sampling is TRUE
    ),
  hr(),
  # App-specific controls
  fluidRow(
    column(3,
           checkboxGroupInput("layers", "Stat Annotations",
                              c("Show means" = "show_mean",
                                "Show conf. intervals" = "show_ci",
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

# Server logic
server <- function(session, input, output) {
  # The boilerplate app
  V <- reactiveValues()
  V$Raw_data <- NHANES
  # reactives for display tabs
  output$plot <- renderPlot({
    req(V$model_formula) # make sure V is initialized
    two_sample_t_plot(V)
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
    V$model_formula <<- as.formula(paste(V$response, "~", V$explan))
  })
  observe({
    # These are specific to this app.
    V$center_on <<- input$center_on # center on left or right mean
    V$equal_var <<- input$equal_var # use equal variance t-test
    V$model_formula <<- as.formula(paste(input$response, "~", input$explan))
  })

  observe({ # T calculations
    tmp <- stats::t.test(V$model_formula, data = V$data,
                         var.equal = V$equal_var)

    if (V$center_on == "Left") {
      res <- tmp$estimate[2] + tmp$conf.int
      #res <- res + abs(diff(tmp$estimate))
    } else {
      res <- tmp$estimate[1] - tmp$conf.int
      #res <- res - abs(diff(tmp$estimate))
    }
    res <- as.list(res)
    names(res) <- c("low", "high")
    res$midpoint <- 1.5
    res$p.value <- tmp$p.value
    V$t_stats <<- as.data.frame(res)
  })

  output$pvalue <- renderText({
    if ("show_t" %in% input$layers ) nice_p(V$t_stats$p.value)
    else ""
  })


  observe({
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
    V$show_coverage <<-
      if ("layers" %in% names(input)) "show_coverage" %in% input$layers
      else FALSE
    V$ci_level <<-
      if ("layers" %in% names(input) && "at_90" %in% input$layers) 0.90
      else 0.95 #default
  })

}

# Run the application
shinyApp(ui = ui, server = server)

