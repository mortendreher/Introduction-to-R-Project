library(shiny)
library(tidyverse)
library(ggplot2)
library(survminer)
library(gmodels)
library(knitr)
library(plotly)
library(gt)
library(papeR)
library(shinythemes)
library(survival)


df <- read.csv("liver_cancer.csv", header = TRUE)
df <- rename(df, ID = X) %>% select(-(X.1))
labels(df) <- c(
  "ID", "Sex", "Age", "Height", "Weight", "BMI", "Diet", "Cholesterol", "Smoker", "Cigs per day", "Packyears", "Alcohol (g/day)",
  "Tumour size", "Bilirubin", "Hepatitis B", "Hepatitis C", "Diabetes"
)

df_alive <- as.data.frame(t(read.csv("sims_alive.csv", header = TRUE)[, 2:13]))
df_bili <- as.data.frame(t(read.csv("sims_bili.csv", header = TRUE)[, 2:13]))
df_bmi <- as.data.frame(t(read.csv("sims_bmi.csv", header = TRUE)[, 2:13]))
df_size <- as.data.frame(t(read.csv("sims_size.csv", header = TRUE)[, 2:13]))
df_weight <- as.data.frame(t(read.csv("sims_weight.csv", header = TRUE)[, 2:13]))

df_alive <- rbind(df_alive, numeric(300), numeric(300))

for (i in (1:300)) {
  if (-1 %in% df_alive[, i]) {
    df_alive[13, i] <- sum(df_alive[, i][df_alive[, i] == 1])
  }
  else {
    df_alive[13, i] <- sum(df_alive[, i])
  }
  if (0 %in% df_alive[1:12, i]) {
    df_alive[14, i] <- 2
  }
  else {
    df_alive[14, i] <- 1
  }
}

df_surv <- as.data.frame(t(df_alive[13:14, ]))
colnames(df_surv) <- c("time", "event")


c_vars_choices <- c(
  "Sex (0=w, 1=m)" = "sex", "Age" = "age", "Height" = "height", "Weight" = "weight", "BMI" = "bmi",
  "Diet" = "diet", "Cholesterol" = "chol", "Smoker" = "smoker", "Cigarettes per day" = "cigs_per_day", "Packyears" = "packyears",
  "Alcohol (g/day)" = "alc", "Tumour size" = "size", "Bilirubin" = "bili", "Hepatitis B" = "hbv", "Hepatitis C" = "hcv", "Diabetes" = "dia"
)

var_choices <- c(
  "Age" = "age", "Height" = "height", "Weight" = "weight", "BMI" = "bmi", "Cholesterol" = "chol",
  "Cigarettes per day" = "cigs_per_day", "Packyears" = "packyears",
  "Alcohol (g/day)" = "alc", "Tumour size" = "size", "Bilirubin" = "bili"
)

var_group_choices <- c("None",
  "Sex" = "sex", "Diet" = "diet", "Smoker" = "smoker",
  "Hepatitis B" = "hbv", "Hepatitis C" = "hcv", "Diabetes" = "dia"
)

var_risk_choices <- c(
  "Sex" = "sex", "Smoker" = "smoker",
  "Hepatitis B" = "hbv", "Hepatitis C" = "hcv", "Diabetes" = "dia"
)

var_sims_choices <- c("Weight" = "weight", "BMI" = "bmi", "Tumour size" = "size", "Bilirubin" = "bili")

unlist_as_char <- function(x, df) {
  result <- as.character(unlist(df[colnames(df) == x]))
  return(result)
}
