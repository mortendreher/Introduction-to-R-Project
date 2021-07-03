library(shiny)
library(knitr)
library(shinythemes)

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
                                            choices = c('Sex (0=w, 1=m)'='sex', 'Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi', 
                                                        'Diet'='diet','Cholesterol'='chol', 'Smoker'='smoker', 'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                        'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili', 'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia')
                                            , selected = c('age','sex','height','weight','bmi','diet','smoker','cigs_per_day', 'packyears', 'alc', 'size', 'bili', 'hbv', 'hcv','dia', 'chol')),
                         ),
                         column(10,dataTableOutput(outputId='out_dataset'))
                         
                         ),
                tabPanel("Graphs",icon = icon("bar-chart-o"),
                         h2("Scatterplot"),
                         sidebarLayout(
                           sidebarPanel(selectInput(inputId = 'scat_var_x', 'Choose variable for x', 
                                                 choices =
                                                 c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi', 'Cholesterol'='chol',
                                                  'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                  'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'bili'),
                                        (selectInput(inputId = 'scat_var_y', 'Choose variable for y',
                                                 choices =
                                                   c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi', 'Cholesterol'='chol',
                                                     'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                     'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'size')),
                                        (selectInput(inputId = 'scat_var_group', 'Choose grouping variable',
                                                 choices =
                                                 c( 'None', 'Sex' = 'sex', 'Diet' = 'diet', 'Smoker' = 'smoker',
                                                    'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'None'))),
                           mainPanel(plotOutput("scatter"))
                         ),
                         h2("Boxplot"),
                         sidebarLayout(sidebarPanel(selectInput(inputId = 'box_var_x', 'Choose variable for x', 
                                                        choices =
                                                          c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi', 'Cholesterol'='chol',
                                                            'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                            'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'bili'),
                                  (selectInput(inputId = 'box_var_group', 'Choose grouping variable',
                                                        choices =
                                                          c( 'None', 'Sex' = 'sex', 'Diet' = 'diet', 'Smoker' = 'smoker',
                                                             'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'None'))),
                                  mainPanel(plotOutput("box"))
                         ),
                         h2("Histogram"),
                         sidebarLayout(sidebarPanel(selectInput(inputId = 'histo_var_x', 'Choose variable for x', 
                                                        choices =
                                                          c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi', 'Cholesterol'='chol',
                                                            'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                            'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'bili')),
                                  mainPanel(plotOutput("histo"))
                         )
                ),
                tabPanel("Tables",icon = icon("table"),
                         h2("Summaries"),
                         sidebarLayout(sidebarPanel(selectInput(inputId = 'tab_sum', 'Choose a metric variable',
                                                        choices = 
                                                          c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi', 'Cholesterol'='chol',
                                                            'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                            'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'bili')
                                                          ),
                                  mainPanel(tableOutput("sum"))
                                  ),
                         h2("Frequencies"),
                         sidebarLayout(sidebarPanel(selectInput(inputId = 'tab_freq_1', ' Choose a categorical variable',
                                                        choices =
                                                          c('Sex' = 'sex', 'Diet' = 'diet', 'Smoker' = 'smoker',
                                                             'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'Diet'),
                                                    selectInput(inputId = 'tab_freq_2', ' Choose a categorical variable',
                                                        choices =
                                                          c( 'None', 'Sex' = 'sex', 'Diet' = 'diet', 'Smoker' = 'smoker',
                                                             'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'None')),
                                  mainPanel(tableOutput("freq"))
                                  ),
                         h2("Risks"),
                         sidebarLayout(sidebarPanel(selectInput(inputId = 'tab_risk_1', ' Choose a categorical variable',
                                                        choices =
                                                          c( 'Sex' = 'sex', 'Smoker' = 'smoker',
                                                             'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'Sex'),
                                        selectInput(inputId = 'tab_risk_2', ' Choose a categorical variable',
                                                        choices =
                                                          c( 'Sex' = 'sex', 'Smoker' = 'smoker',
                                                             'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'hbv')),
                                  mainPanel(tableOutput("risk"))
                                  ),
                         h2("Confidence Intervals"),
                         sidebarLayout(sidebarPanel(selectInput(inputId = 'tab_ci', 'Choose a metric variable',
                                                        choices =
                                                          c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi', 'Cholesterol'='chol',
                                                            'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                            'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'bili')
                                                          ,
                                  sliderInput(inputId = 'slider_ci', ' Choose CI percentage',
                                                        value = 0.95, min = 0, max = 1, step = 0.01)),
                                  mainPanel(tableOutput("ci"))
                         )
)
)