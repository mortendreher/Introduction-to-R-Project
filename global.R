c_vars_choices <- c('Sex (0=w, 1=m)'='sex', 'Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi', 
                   'Diet'='diet','Cholesterol'='chol', 'Smoker'='smoker', 'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                   'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili', 'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia')

var_choices <- c('Age' = 'age','Height' = 'height', 'Weight' = 'weight', 'BMI' = 'bmi', 'Cholesterol'='chol',
                        'Cigarettes per day' = 'cigs_per_day', 'Packyears' = 'packyears',
                        'Alcohol (g/day)'= 'alc','Tumour size' = 'size', 'Bilirubin' = 'bili')

var_group_choices <- c( 'None', 'Sex' = 'sex', 'Diet' = 'diet', 'Smoker' = 'smoker',
                             'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia')

var_risk_choices <- c( 'Sex' = 'sex', 'Smoker' = 'smoker',
                       'Hepatitis B' = 'hbv', 'Hepatitis C' = 'hcv', 'Diabetes' = 'dia')

unlist_as_char <- function(x, df) {
  result <- as.character(unlist(df[colnames(df) == x]))
  return(result)
}



