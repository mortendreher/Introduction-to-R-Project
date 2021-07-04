library(dplyr)

alc <- round(rchisq(n = 300, df = 4) * 25, digits = 1)

cigs_per_day <- floor(runif(n = 300, min = 1, max = 6)) * 10 * smoker

packyears <- (cigs_per_day / 20) * (age - round(runif(n = 12, min = 16, max = min(age)), digits = 2))


hbv <- rbinom(n = 300, size = 1, prob = .4)
hcv <- rbinom(n = 300, size = 1, prob = .6)
dia <- rbinom(n = 300, size = 1, prob = .5)

bmi <- round(weight / ((height / 100) * 2), digits = 1)

bmi_class <- case_when(
  bmi < 22 ~ 1,
  bmi >= 22 & bmi < 24 ~ 2,
  bmi >= 24 & bmi < 26 ~ 3,
  bmi >= 26 ~ 4
)

mean(chol[bmi_class == 1], na.rm = TRUE)
mean(chol[bmi_class == 2], na.rm = TRUE)
mean(chol[bmi_class == 3], na.rm = TRUE)


chol[50] <- round(mean(chol[bmi_class == bmi_class[50]], na.rm = TRUE), digits = 2)

chol[72] <- round(mean(chol[bmi_class == bmi_class[72]], na.rm = TRUE), digits = 2)

chol[182] <- round(mean(chol[bmi_class == bmi_class[182]], na.rm = TRUE), digits = 2)

bili <- round(1.8 + chol / 30 + size / 5 + alc / 100 + packyears / 100, digits = 2)