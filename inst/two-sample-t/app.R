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

main_plot_function <-  function(state) {
  F <- as.formula(paste(state$response, "~", state$explan))
  COLOR <- as.formula(paste("~", state$explan))
  do.call(gf_jitter, list(F, data = state$data,
                          color = COLOR,
                          width = 0.2,
                          alpha = point_alpha(state$samp_n))) %>%
    gf_theme(legend.position = "top") %>%
    gf_lims(y = state$y_range) %>%
    stats_annot_function(F, COLOR, state)
}


# P **must** be first
stats_annot_function <- function(P, F, COLOR, state) {
  this_data <- state$data
  stats <-
    if (state$ci_level == 0.95) df_stats(F, data = this_data,
                                         mn = mean,
                                ci = ci.mean(level = 0.95))
    else df_stats(F, data = this_data,
                  mn = mean,
                  ci = ci.mean(level = 0.90))
    stats <- stats %>% mutate(xpos = c(1.25, 1.75))
    if (state$show_mean) {
      P <- do.call(gf_errorbar, list(P, mn + mn ~ xpos,
                                     data = stats,
                                     color = COLOR,
                                     width = 0.2,
                                     size = 2,
                                     show.legend = FALSE))
    }
  ebar_formula <- as.formula(paste("ci_lower + ci_upper ~", state$explan))

  if (state$show_ci) {
    P <- do.call(gf_errorbar, list(P, ebar_formula,
                data = stats,
                color = COLOR,
                width = 0.13,
                size = 1.5,
                show.legend = FALSE))
  }
  if (state$show_t) {
    tstats <- state$t_stats
    P <- P %>%
      gf_errorbar(low + high ~ midpoint, data = tstats,
                  width = 1, show.legend = FALSE)
  }
  set.seed(NULL)

  P
}






# Define UI for application that draws a histogram
ui <- fluidPage(
  main_display(
    response_vars = get_var_names(NHANES, type = "quantitative"),
    explan_vars = get_var_names(NHANES, type = "categorical", max_levels = 2),
    covars = list("Not relevant to t statistic." = ""), #get_var_names(NHANES, type = "all"),
    multiple_covariates = TRUE,
    balance_groups = TRUE
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

# Use boilerplate server
server <- function(session, input, output) {
  # The boilerplate app
  V <- reactiveValues()
  standard_littleapp_server(session, input, output, V)
  observe({
    # These are specific to this app.
    V$center_on <<- input$center_on
    V$equal_var <<- input$equal_var
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

