library(shiny)
library(knitr)
library(shinythemes)
source("global.R")

rmdfiles <- c("about_liver.rmd")
sapply(rmdfiles, knit, quiet = T)

ui <- navbarPage(theme = shinytheme("slate"),"Liver cancer",
                tabPanel("Team members", "By Melita Coneva, Morten Dreher and Lars Andersen",icon = icon("users")),
                tabPanel("About liver cancer",icon = icon("info"),
                         withMathJax(includeMarkdown("about_liver.md"))
                         ),
                tabPanel("Dataset",icon = icon("list-alt"), fluidRow(h2("Dataset")), 
                         column(2,checkboxInput(inputId='c_all', 'Select all',value = TRUE),
                         checkboxGroupInput(inputId='c_vars', label="Select variables", 
                                            choices = c_vars_choices
                                            , selected = c_vars_choices),
                         ),
                         column(10,dataTableOutput(outputId='out_dataset'))
                         
                         ),
                tabPanel("Graphs",icon = icon("bar-chart-o"),
                         fluidRow(h2("Scatterplot"),
                           column(2, selectInput(inputId = 'scat_var_x', 'Choose variable for x', 
                                                 choices = var_choices, selected = 'bili')),
                           column(2, selectInput(inputId = 'scat_var_y', 'Choose variable for y',
                                                 choices = var_choices, selected = 'size')),
                           column(2, selectInput(inputId = 'scat_var_group', 'Choose grouping variable',
                                                 choices = var_group_choices, selected = 'None')),
                           column(6, plotOutput("scatter"))
                         ),
                         fluidRow(h2("Boxplot"),
                                  column(2, selectInput(inputId = 'box_var_x', 'Choose variable for x', 
                                                        choices = var_choices, selected = 'bili')),
                                  column(2, selectInput(inputId = 'box_var_group', 'Choose grouping variable',
                                                        choices = var_group_choices, selected = 'None')),
                                  column(6, offset = 2, plotOutput("box"))
                         ),
                         fluidRow(h2("Histogram"),
                                  column(2, selectInput(inputId = 'histo_var_x', 'Choose variable for x', 
                                                        choices = var_choices, selected = 'bili')),
                                  column(6, offset = 4, plotOutput("histo"))
                         ),
                ),
                tabPanel("Tables",icon = icon("table"),
                         fluidRow(h2("Summaries"),
                                  column(2, selectInput(inputId = 'tab_sum', 'Choose a metric variable',
                                                        choices = var_choices, selected = 'bili')
                                                          ),
                                  column(6, offset = 4, tableOutput("sum"))
                                  ),
                         fluidRow(h2("Frequencies"),
                                  column(2, selectInput(inputId = 'tab_freq_1', ' Choose a categorical variable',
                                                        choices = var_group_choices, selected = 'Diet')),
                                  column(2, selectInput(inputId = 'tab_freq_2', ' Choose a categorical variable',
                                                        choices = var_group_choices, selected = 'Sex')),
                                  column(6, offset = 2, tableOutput("freq"))
                                  ),
                         fluidRow(h2("Risks"),
                                  column(2, selectInput(inputId = 'tab_risk_1', ' Choose a categorical variable',
                                                        choices = var_risk_choices, selected = 'Sex')),
                                  column(2, selectInput(inputId = 'tab_risk_2', ' Choose a categorical variable',
                                                        choices = var_risk_choices, selected = 'hbv')),
                                  column(6, offset = 2, tableOutput("risk"))
                                  ),
                         fluidRow(h2("Confidence Intervals"),
                                  column(2, selectInput(inputId = 'tab_ci', 'Choose a metric variable',
                                                        choices = var_choices, selected = 'bili')
                                                          ),
                                  column(2, sliderInput(inputId = 'slider_ci', ' Choose CI percentage',
                                                        value = 0.95, min = 0, max = 1, step = 0.01)),
                                  column(6, offset = 2, tableOutput("ci"))
                         )
)
)