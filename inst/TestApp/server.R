library(shiny)
library(StatPREPshiny)
library(mosaicModel)
library(ggformula)
library(dplyr)
library(NHANES)
library(splines)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  model_formula <- reactive({
    base_str <- paste(input$response, "~ ns(", input$explan, ", ", input$smoothing, ")")
    if ("NONE" != input$covar)
      base_str <- paste(base_str, is_interaction(), input$covar)

    as.formula(base_str)
  })
  frame_formula <- reactive({
    as.formula(paste(input$response, "~", input$explan))
  })
  color_formula <- reactive({
    if (input$covar == "NONE") "black"
    else as.formula(paste("~", input$covar))
  })
  is_interaction <- reactive({
    ifelse(input$interaction, "*", "+")
  })
  fit_model <- reactive({
    lm(model_formula(), data = get_sample())
  })
  ebar_width <- reactive({
    x_range <- range(get_data()[[input$explan]], na.rm = TRUE)
    diff(x_range) / 15
  })

  get_data  <- reactive({
    NHANES
  })
  samp_size <- reactive({
    as.numeric(input$samp_n)
  })
  get_sample<- reactive({
    set.seed(input$seed)
    vars <- c(input$response, input$explan)
    if("NONE" != input$covar) vars <- c(vars, input$covar)
    res <- get_data()[vars] %>% na.omit() %>% sample_n(size = samp_size())
    if (input$covar != "NONE")
      res[["point_color"]] <- res[[input$covar]] # for plotting
    else res[["point_color"]] <- "black"

    res
  })
  scatter_plot <- reactive({
    gf_point(frame_formula(), data = get_sample(), color = color_formula())
  })
  regression_report <- reactive({paste(capture.output(summary(fit_model()))[-(1:8)], collapse = "\n")})
  anova_report <- reactive({paste(capture.output(anova(fit_model())), collapse = "\n")})

  graphics <- reactive({
    # Get the model functions for plotting
    mfundata <- mod_eval(fit_model(), nlevels = Inf, interval = input$interval_type)

    mod_vals <- mod_eval(fit_model(), data = get_sample())
    x_range <- range(mfundata[[input$explan]])
    x_max <- x_range[2] + 0.25 * diff(x_range)
    x_mod_vals <- x_range[2] + 0.1 * diff(x_range)
    x_raw_vals <- x_range[2] + 0.2 * diff(x_range)
    mod_vals$x_mod_vals <- x_mod_vals
    mod_vals$x_raw_vals <- x_raw_vals
    mod_vals$raw <- get_sample()[[input$response]]
    ebar_width <- diff(x_range)/15
    std_bars <- data.frame(base = rep(mean(mod_vals$raw, na.rm = TRUE), 2),
                           x = c(x_mod_vals, x_raw_vals))
    std_bars$top <- std_bars$base + c(sd(mod_vals$model_output, na.rm = TRUE),
                                      sd(mod_vals$raw, na.rm = TRUE))

    P <- scatter_plot() %>%
      gf_line(as.formula(paste("model_output ~ ", input$explan)),
              data = mfundata, color = color_formula()) %>%
      gf_theme(legend.position = "top")

    P <- P %>% gf_errorbar(model_output + model_output ~ x_mod_vals, data = mod_vals,
                           width = ebar_width(), color = "red") %>%
      gf_errorbar(raw + raw ~ x_raw_vals, data = mod_vals,
                  width = ebar_width(), color = "red")

    # # confidence bands
    # # gf_ribbon() not working for some unknown reason.
    if (input$interval_type != "none") {
      P <- P %>% gf_errorbar(data = mfundata,
                           as.formula(paste("lower + upper ~ ", input$explan)),
                           alpha = 0.2, fill = color_formula(), color = color_formula())
    }

    if (input$show_mod_values)
      P <- P %>% gf_point(as.formula(paste("model_output ~", input$explan)),
                          data = mod_vals, color = color_formula(), shape = 2)

    if (input$trace_horizontally)
      P <- P %>% gf_segment(
        as.formula(paste("model_output + model_output ~ ", input$explan, "+ x_mod_vals")),
        data = mod_vals,
        color = "black", size = 0.1)

    if (input$show_resids)
      P <- P %>% gf_segment(as.formula(paste("model_output + raw ~ ",
                                             input$explan, "+", input$explan)),
                            data = mod_vals,
                            color = "black", size = 0.1)

    P <- P %>% gf_segment(base + top ~ x + x, data = std_bars,
                          size = ebar_width()/2, color = "black", alpha = 0.5)

    P
  })

  output$plot <- renderPlot({graphics()})
  output$anova_report <- renderText({anova_report()})
  output$regression_report <- renderText({regression_report()})
  output$explain <- renderText({
    # HTML(includeMarkdown("../t-statistic-explain.md"))
    })

})
