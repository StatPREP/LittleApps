library(shiny)
library(LittleApps)
library(ggformula)
library(NHANES)
library(dplyr)

explanatory_vars <- c("Gender", "HomeOwn", "SmokeNow", "SexEver", "PregnantNow", "Depressed", "Work", "PhysActive" )
response_vars <- c("Age", "Poverty", "Pulse", "BMI", "TotChol", "BPSys1", "UrineVol1", "Weight", "Height", "DaysMentHlthBad", "LittleInterest", "Age1stBaby", "PhysActiveDays", "SexNumPartnLife")
NHANES <- NHANES %>% filter(Age >= 18)

P <- plotOutput("plot1",
                brush = brushOpts("horiz_line", direction = "y", fill = NA))
EXPLAIN <- htmlOutput("explain")
CODEBOOK <- verbatimTextOutput("codebook")
DISPLAY <- tabsetPanel(tabPanel("Main graphic", P),
                       tabPanel("Explain annotations", EXPLAIN),
                       tabPanel("Codebook", CODEBOOK))
keepers <- list(
  PregnantNow = c("Yes", "No"),
  SmokeNow = c("Yes", "No"),
  HomeOwn = c("Own", "Rent"),
  Depressed = c("None", "Most"),
  Work = c("Working", "NotWorking"),
  PhysActive = c("Yes", "No"),
  SexEver = c("Yes", "No"),
  Gender = c("male", "female"),
  LittleInterest = c("None", "AlmostAll")
)



ui <- fluidPage(
  h2("Two-sample t test"),
  fluidRow(
    column(2,
           selectInput('response', 'Response Variable', response_vars),
           selectInput('explan', 'Explanatory Var.', explanatory_vars),
           selectInput('covar', 'Covariates', c('None')),
           selectInput("samp_n", "Sample size", choices = c(5, 10, 20, 50, 100, 500, 1000),
                       selected = "50"),
           hr(),
           actionButton("seed", "Take new sample")),
    column(6,DISPLAY)
    ),
  hr(),
  # controls
  fluidRow(

    column(3,
           checkboxGroupInput("layers", "Stat Annotations",
                              c("Show conf. intervals",
                                "Show t interval",
                                "CIs @ 90% confidence level"))
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
                              "Shuffle explanatory var."))
  )
)

server <- function(input, output, session) {
  show_t <- reactive({"Show t interval" %in% input$layers})
  show_ci <- reactive({"Show conf. intervals" %in% input$layers})
  show_90_level <- reactive({"CIs @ 90% confidence level" %in% input$layers})
  use_shuffle <- reactive("Shuffle explanatory var." %in% input$simulation)

  the_formula <- reactive({
    as.formula(paste(req(input$response), "~", req(input$explan)))
  })
  get_data <- reactive({
    res <- NHANES[, all.vars(req(the_formula()))] %>% na.omit()
    res[res[[req(input$explan)]] %in% keepers[[input$explan]], ]
  })
  samp_n <- reactive( {
    as.numeric(input$samp_n)
  })
  get_samp <- reactive( {
    set.seed(as.numeric(input$seed))
    res <- get_data() %>% sample_n(size = samp_n())
    if (use_shuffle()) res[[input$explan]] <- sample(res[[input$explan]])

    res
  })


  color_formula <- reactive( {
    as.formula(paste("~", input$explan))
  })


  stats_interval <- reactive( {
    this_level <- ifelse(show_90_level(), .9, .95)
    df_stats(the_formula(), data = get_samp(),
             mn = mean, ci = ci.mean(level = !!this_level), long_names = FALSE) %>%
      mutate(xpos = c(1.25, 1.75))
  })

  output$pvalue <- renderText({
    if (show_t()) nice_p(t_interval()$p.value)
    else ""})

  t_interval <- reactive( {
    tmp <- stats::t.test(the_formula(), data = get_samp(), var.equal = input$equal_var)

    if (input$center_on == "Left") {
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
    as.data.frame(res)
  })

  graphics <- reactive( {
    set.seed(101)
    width_val <- 0.1

    P <- gf_jitter(the_formula(), data = get_samp(),
                   color = color_formula(),
                   alpha = point_alpha(samp_n()),
                   width = 0.1,
                   height = 0,
                   seed = 12345,
                   show.legend = FALSE)

    P <- P %>% gf_errorbar(mn + mn ~ xpos,
                           data = stats_interval(), color = color_formula(),
                           show.legend = FALSE,
                           width = 3 * 0.1, size=2, alpha = .5)
    if (show_ci()) {
      P <- P %>%
        gf_errorbar(as.formula(paste("ci_lower + ci_upper ~ ", input$explan)),
                    data = stats_interval(),
                    color = color_formula(),
                    width = 1.3 * 0.1, size = 2, show.legend = FALSE)
    }

    if (show_t()) {
      tstats <- t_interval()
      P <- P %>%
        gf_errorbar(low + high ~ midpoint, data = tstats,
                    width = 1, show.legend = FALSE)
    }
    set.seed(NULL)
    P %>% gf_lims(y = range(get_data()[[input$response]])) %>%
      gf_labs(x = ifelse(use_shuffle(), "Shuffled", input$explan))
  })

  output$plot1 <- renderPlot(graphics())
  output$codebook <- renderText({
      LA_var_help("NHANES", input$response, input$explan, line_length = 40L)
  })
  output$explain <- renderText({
    HTML(includeMarkdown("t-statistic-explain.md"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

