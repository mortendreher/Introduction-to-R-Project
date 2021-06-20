library(shiny)

ui <- navbarPage("Liver cancer",
                tabPanel("Team members", "By Melita Coneva, Morten Dreher and Lars Andersen"),
                tabPanel("About liver cancer", 
                         p(h3("Overview"),"The liver is the central metabolism organ of the human body.", br(), 
                          "It is located in the upper right side of the body, protected by the lower ribs. With a weight of approximately 1500 grams, it is the largest gland of the body.",br(),
                          "Besides metabolism, the liver is also responsible for producing a variety of coagulation factors and hormones. The organ is also essential for removing external substances like alcohol and medications from the blood stream.", br(), br(),
                         "For 2020, the WHO estimated the number of new cases of liver cancer (ICD C22) to be 905 677 (CI:[884695-927157]).", br(),
                         "According to the WHO, liver cancer caused 830 000 deaths worldwide in 2020, a number only exceeded by lung cancer (1.8 million deaths worldwide) and colorectal cancer (935 000 deaths worldwide)."),
                         p(h3("Epidemiology"), "In most sources, liver cancer prevalence, incidence and mortality for men is about 2-3 times as high as it is for women.", "WHO data estimates that in 2020, 632 000 men and 273 000 women were 
                          diagnosed with liver cancer and that in the same year, 578 000 men and 253 000 women in association with liver cancer.", "For men, liver cancer mortality is only surpassed by lung cancer mortality.",
                          "For women on the other hand, liver cancer only takes sixth place in terms of total deaths, with sex-specific tumours like breast and uterus cancer accounting for more deaths.", br(),
                          "For Germany, the most recent data available describes new cases and mortality in 2017. In this year, 6040 men and 2903 women were diagnosed with liver cancer and 5213 men and 2697 women died in association with liver cancer."),
                         p(h3("Risk factors"), "Chronic hepatitis B or hepatitis C infection greatly increase the risk of developing liver cancer.", 
                          "Cirrhosis, an irreversible process in which healthy liver tissue is replaced by scar tissue, thus losing its functions, is also a prominent risk factor for liver cancer.",
                          "Excessive alcohol consumption, obesity, diabetes and nonalcoholic fatty liver disease are also associated with an increased risk for liver cancer."),
                         p(h3("Symptoms")),
                         p(h3("Diagnosis")),
                         p(h3("Treatment")),
                         p(h3("Prevention"), "As HBV and HCV infections are major risk factors for the development of liver cancer, taking precautions against these viruses can help prevent liver cancer. 
                             Protection against hepatitis B can be obtained by a vaccine which is recommended for children and adults at risk in many countries including Germany, the US and the UK.", br(),
                         "Hepatitis C is transmitted parenterally and sexually, therefore prevention includes only using clean, disinfected needles and syringes as well as being aware of the health status of sexual partners.", br(),
                         "In the case that an infection with HBV or HCV could not be prevented, treatments reducing liver cancer risk are available.", br(), br(),
                         "In a 2007 meta-analysis, increased coffee consumption was found to be associated to a lower risk of liver cancer.", br(), br(),
                         "Reducing alcohol consumption and keeping a healthy weight can also help reduce liver cancer risk."),
                         p(h3("Sources"),
                             tags$ul(
                               tags$li("https://www.nhs.uk/conditions/liver-cancer/"),
                               tags$li("https://www.who.int/news-room/fact-sheets/detail/cancer"),
                               tags$li("https://www.wcrf.org/dietandcancer/liver-cancer-statistics/"),
                               tags$li("https://www.wcrf.org/dietandcancer/liver-cancer/"),
                               tags$li("https://www.cdc.gov/cancer/liver/index.htm"),
                               tags$li("https://www.krebsdaten.de/Krebs/DE/Content/Krebsarten/Leberkrebs/leberkrebs_node.html"),
                               tags$li("https://www.mayoclinic.org/diseases-conditions/liver-cancer/symptoms-causes/syc-20353659"),
                               tags$li("https://www.sciencedirect.com/science/article/abs/pii/S0016508507005689")
                               )
                             )
                         ),
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