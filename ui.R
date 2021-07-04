rmdfiles <- c("about_liver.rmd")
sapply(rmdfiles, knit, quiet = T)

ui <- navbarPage(
  theme = shinytheme("slate"), "Liver cancer",
  tabPanel("About App", "Melita Coneva, Morten Dreher and Lars Andersen", tags$br(),
    tags$a(href = "https://github.com/mortendreher/Introduction-to-R-Project", "Git Repository"),
    icon = icon("users")
  ),
  tabPanel("About liver cancer",
    icon = icon("info"),
    withMathJax(includeMarkdown("about_liver.md"))
  ),
  tabPanel("Dataset",
    icon = icon("list-alt"), fluidRow(h2("Dataset")),
    column(
      2, checkboxInput(inputId = "c_all", "Select all", value = TRUE),
      checkboxGroupInput(
        inputId = "c_vars", label = "Select variables",
        choices = c_vars_choices, selected = c_vars_choices
      ),
    ),
    column(10, dataTableOutput(outputId = "out_dataset"))
  ),
  tabPanel("Graphs",
    icon = icon("bar-chart-o"),
    h2("Scatterplot"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "scat_var_x", "Choose variable for x",
          choices = var_choices, selected = "bili"
        ),
        (selectInput(
          inputId = "scat_var_y", "Choose variable for y",
          choices = var_choices, selected = "size"
        )),
        (selectInput(
          inputId = "scat_var_group", "Choose grouping variable",
          choices = var_group_choices, selected = "None"
        ))
      ),
      mainPanel(plotlyOutput("scatter"))
    ),
    h2("Boxplot"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "box_var_x", "Choose variable for x",
          choices = var_choices, selected = "bili"
        ),
        (selectInput(
          inputId = "box_var_group", "Choose grouping variable",
          choices = var_group_choices, selected = "None"
        ))
      ),
      mainPanel(plotOutput("box"))
    ),
    h2("Histogram"),
    sidebarLayout(
      sidebarPanel(selectInput(
        inputId = "histo_var_x", "Choose variable for x",
        choices = var_choices, selected = "bili"
      )),
      mainPanel(plotOutput("histo"))
    )
  ),
  tabPanel("Tables",
    icon = icon("table"),
    h2("Summaries"),
    sidebarLayout(
      sidebarPanel(selectInput(
        inputId = "tab_sum", "Choose a metric variable",
        choices = var_choices, selected = "bili"
      )),
      mainPanel(tableOutput("sum"))
    ),
    h2("Frequencies"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "tab_freq_1", " Choose a categorical variable",
          choices = var_risk_choices, selected = "Diet"
        ),
        selectInput(
          inputId = "tab_freq_2", " Choose a categorical variable",
          choices = var_group_choices, selected = "None"
        )
      ),
      mainPanel(gt_output("freq"))
    ),
    h2("Risks"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "tab_risk_1", " Choose a categorical variable",
          choices = var_risk_choices, selected = "Sex"
        ),
        selectInput(
          inputId = "tab_risk_2", " Choose a categorical variable",
          choices = var_risk_choices, selected = "hbv"
        )
      ),
      mainPanel(gt_output("risk"))
    ),
    h2("Confidence Intervals (based on t-Test statistic)"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "tab_ci", "Choose a metric variable",
          choices = var_choices, selected = "bili"
        ),
        sliderInput(
          inputId = "slider_ci", " Choose CI percentage",
          value = 0.95, min = 0, max = 1, step = 0.01
        )
      ),
      mainPanel(tableOutput("ci"))
    )
  ),
  tabPanel("Survival Analysis",
    icon = icon("poll"),
    h2("Kaplan-Meier-Curve"),
    fluidRow(column(8, plotOutput("plot_km"))),
    h2("Simulated variables"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "sims", "Choose variable to simulate", choices = var_sims_choices),
        numericInput(inputId = "sims_pat", "Patient ID (1-300)", min = 1, max = 300, value = 1, step = 1)
      ),
      mainPanel(plotlyOutput("plot_sims"))
    )
  )
)