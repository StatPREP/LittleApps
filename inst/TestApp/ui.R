library(shiny)

covariates <- c("Gender", "HomeOwn", "SmokeNow", "PregnantNow", "Depressed", "Work", "PhysActive" )
response_vars <- c("Age", "Poverty", "Pulse", "BMI", "TotChol", "BPSys1", "UrineVol1", "Weight", "Height", "DaysMentHlthBad", "LittleInterest", "Age1stBaby", "PhysActiveDays", "SexNumPartnLife")

RESPONSE <- selectInput("response", "Response variable",
                        choices = response_vars, selected = "Height", width = "150px")
EXPLAN <- selectInput("explan", "Explanatory variable",
                      choices = response_vars, selected = "Age", width = "150px")
COVAR <- selectInput("covar", "Covariate",
                     choices = c("NONE", as.list(covariates)),
                     selected = "Gender", width = "150px")
SAMP_N <- selectInput("samp_n", "Sample size:", choices = c(5, 10, 20, 50, 100, 500, 1000),
                      selected = "50", width = "100px")
INTERACTION <- checkboxInput("interaction", "Include interaction", TRUE, width = "150px")
SMOOTHING <- selectInput("smoothing", "Smoothing/Curviness",
                         choices = 1:6, width = "150px")
INTERVALS <- selectInput("interval_type", "Interval type",
                         choices = c("none", "confidence", "prediction"),
                         width = "150px")
NEW_TRIAL <- actionButton("seed", "Take new sample")
SHOW_MOD_VALUES <- checkboxInput("show_mod_values", "show model values", width = "200px")
TRACE_H <- checkboxInput("trace_horizontally", "trace horizontally", width = "200px")
TRACE_V <- checkboxInput("show_resids", "trace vertically", width = "150px")
PLOT <- plotOutput("plot", width = "500px",
                   brush = brushOpts("horiz_line", direction = "x", fill = NA))
ANOVA <- verbatimTextOutput("anova_report")
EXPLAIN <- htmlOutput("explain")
EXPLAIN2 <- withMathJax(HTML(includeMarkdown("../t-statistic-explain.md")))
REGRESSION <- verbatimTextOutput("regression_report")
DISPLAY <- tabsetPanel(tabPanel("Graphics", PLOT),
                       tabPanel("ANOVA report", ANOVA),
                       tabPanel("Explain", EXPLAIN2) ,
                       tabPanel("Regression report", REGRESSION))


# Define UI for application that draws a histogram
# shinyUI(dashboardPage(
#
#   # Application title
#   titlePanel("Regression models"),
#
#   # Sidebar with a slider input for number of bins
#   dashboardSidebar(
#     sidebarPanel(
#        RESPONSE, EXPLAN, COVAR, SAMP_N, INTERACTION, SMOOTHING, INTERVALS, NEW_TRIAL,
#        SHOW_MOD_VALUES, TRACE_H, TRACE_V
#     ),
#
#     # Show a plot of the generated distribution
#     dashboardBody(
#        DISPLAY
#     )
#   )
# ))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("graphics", tabName = "Graphics", icon = icon("picture", lib = "glyphicon")),
    menuItem("ANOVA", icon = icon("signal", lib = "glyphicon"), tabName = "ANOVA",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("regression", icon = icon("align-justify", lib = "glyphicon"), tabName = "Regression"),
    menuItem("Explain", icon = icon("picture", lib = "glyphicon"), tabName = "Explain"),
    # RESPONSE, EXPLAN,COVAR, INTERACTION
    COVAR, SAMP_N,  SMOOTHING, INTERVALS, NEW_TRIAL,
       SHOW_MOD_VALUES, TRACE_H, TRACE_V
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Graphics", PLOT),
    tabItem(tabName = "ANOVA", ANOVA),
    tabItem(tabName = "Regression", REGRESSION),
    tabItem(tabName = "Explain", EXPLAIN2)
  ),
  fluidRow(box(RESPONSE, background = "black", width = 3),
           box(EXPLAN, background = "black", width = 4),
           box(COVAR, background = "black", width = 3),
           box(INTERACTION, background = "black", width = 2))
)

dashboardPage(
  dashboardHeader(
       dropdownMenu(type = "tasks", badgeStatus = "danger",
                    tags$li(COVAR),
          taskItem(value = 20, color = "red", "Write documentation")),
       title = "My dashboard"),
  sidebar,
  body
)
