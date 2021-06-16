library(shiny)

ui <- navbarPage("Liverpool",
                tabPanel("Team members", "By Melita Coneva, Morten Dreher and Lars Andersen"),
                tabPanel("About liver cancer", p(tags$a(href="https://flexikon.doccheck.com/de/Lebertumor", "doccheck"))),
                tabPanel("Graphs",
                         fluidRow(h2("Scatterplot"),
                           column(2, selectInput(inputId = 'scat_var_x', 'Choose variable for x', 
                                                 choices =
                                                 c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi',
                                                  'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                  'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'bili')),
                           column(2, selectInput(inputId = 'scat_var_y', 'Choose variable for y',
                                                 choices =
                                                   c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi',
                                                     'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                     'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'size')),
                           column(2, selectInput(inputId = 'scat_var_group', 'Choose grouping variable',
                                                 choices =
                                                 c( 'None', 'Sex' = 'sex', 'Diet' = 'diet', 'Smoker' = 'smoker',
                                                    'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'None')),
                           column(6, plotOutput("scatter"))
                         ),
                         fluidRow(h2("Boxplot"),
                                  column(2, selectInput(inputId = 'box_var_x', 'Choose variable for x', 
                                                        choices =
                                                          c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi',
                                                            'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                            'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'bili')),
                                  column(2, selectInput(inputId = 'box_var_group', 'Choose grouping variable',
                                                        choices =
                                                          c( 'None', 'Sex' = 'sex', 'Diet' = 'diet', 'Smoker' = 'smoker',
                                                             'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'None')),
                                  column(6, offset = 2, plotOutput("box"))
                         ),
                         fluidRow(h2("Histogram"),
                                  column(2, selectInput(inputId = 'histo_var_x', 'Choose variable for x', 
                                                        choices =
                                                          c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi',
                                                            'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                            'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'bili')),
                                  column(6, offset = 4, plotOutput("histo"))
                         ),
                ),
                tabPanel("Tables",
                         fluidRow(h2("Summaries"),
                                  column(2, selectInput(inputId = 'tab_sum', 'Choose a metric variable',
                                                        choices = 
                                                          c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi',
                                                            'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                                                            'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'bili')
                                                          ),
                                  column(6, offset = 4, tableOutput("sum"))
                                  ),
                         fluidRow(h2("Frequencies"),
                                  column(2, selectInput(inputId = 'tab_freq_1', ' Choose a categorical variable',
                                                        choices =
                                                          c( 'None', 'Sex' = 'sex', 'Diet' = 'diet', 'Smoker' = 'smoker',
                                                             'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'None')),
                                  column(2, selectInput(inputId = 'tab_freq_2', ' Choose a categorical variable',
                                                        choices =
                                                          c( 'None', 'Sex' = 'sex', 'Diet' = 'diet', 'Smoker' = 'smoker',
                                                             'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'None')),
                                  column(6, offset = 2, tableOutput("freq"))
                                  ),
                         fluidRow(h2("Risks"),
                                  column(2, selectInput(inputId = 'tab_risk_1', ' Choose a categorical variable',
                                                        choices =
                                                          c( 'None', 'Sex' = 'sex', 'Diet' = 'diet', 'Smoker' = 'smoker',
                                                             'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'None')),
                                  column(2, selectInput(inputId = 'tab_risk_2', ' Choose a categorical variable',
                                                        choices =
                                                          c( 'None', 'Sex' = 'sex', 'Diet' = 'diet', 'Smoker' = 'smoker',
                                                             'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia'), selected = 'None')),
                                  column(6, offset = 2, tableOutput("risk"))
                                  )
                         # fluidRow(h2("Confidence Intervalls"),
                         #          column(2, selectInput(inputId = 'tab_ci', 'Choose a metric variable',
                         #                                choices =
                         #                                  c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi',
                         #                                    'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                         #                                    'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili'), selected = 'bili')
                         #                                  ),
                         #          column(2, sliderInput(inputId = 'slider_ci', ' Choose alpha',
                         #                                value = 0.05, min = 0, max = 1, step = 0.005)),
                         #          column(6, offset = 2, tableOutput("ci"))
                         )
)