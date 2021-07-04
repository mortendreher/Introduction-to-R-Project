library(papeR)
library(dplyr)
library(ggplot2)
# Imputation method: KNN

# variables to be used: sex, age, bmi, size
# computes distance between a given data frame entry and all other entries
# variables to be considered and their weights can be specified

distances <- function(df, id, vars, weights = rep.int(1, length(vars))) {
  distance_to_id <- numeric(length(df[, 1]))
  distance_to_id[id] <- Inf # ensures that node itself will not be returned

  for (v in vars) {
    for (i in (1:length(distance_to_id))) {
      if (i != id) {
        rng <- range(df[, v])
        distance_to_id[i] <- distance_to_id[i] + (abs(df[i, v] - df[id, v]) / (rng[2] - rng[1])) * weights[which(vars == v)]
      }
    }
  }
  return(distance_to_id)
}

knn <- function(df, id, vars, k, weights) {
  distances_to_id <- distances(df, id, vars, weights)
  df_with_distances <- data.frame(cbind(df, distances_to_id), ordered_id = (1:length(distances_to_id)))
  df_with_distances <- filter(df_with_distances, ordered_id != id)
  df_with_distances <- arrange(df_with_distances, df_with_distances$distances_to_id)
  return(df_with_distances$ordered_id[1:k])
}

var <- c(2, 3, 6, 13)
w <- c(0.25, 1, 1.5, 1)
k <- c(3, 5, 7, 9, 11)

for (i in k) {
  print(paste("50 ", i, sd(df$chol[knn(50, i)]))) # min: k=5
  print(paste("72 ", i, sd(df$chol[knn(72, i)]))) # min: k=3
  print(paste("182 ", i, sd(df$chol[knn(182, i)]))) # min: k=9
  print("---------")
}

# choose k=3

# --------------------
# PLOTS
# ------------
# Scatterplot

xvar <- 4
yvar <- 14
classvar <- 7

# example of a dynamic scatter plot. user input will determine x, y and class variables
# reactive: colour TRUE / FALSE

plot_title <- paste0("Variables ", colnames(df[xvar]), " and ", colnames(df[yvar]))
plot_title
ggplot(data = df, mapping = aes(x = df[, xvar], y = df[, yvar])) +
  geom_point(mapping = aes(colour = df[, classvar])) +
  labs(
    title = paste0("Variables ", colnames(df[xvar]), " and ", colnames(df[yvar])), x = colnames(df[xvar]),
    y = colnames(df[yvar]), colour = colnames(df[classvar])
  )

# Boxplot

# consider adding character variable for sex

classvar <- 2

ggplot(data = df) +
  geom_boxplot(mapping = aes(x = df[, xvar], y = df[, classvar], group = df[, classvar])) +
  labs(
    title = paste0("Variables ", colnames(df[xvar]), " and ", colnames(df[classvar])), x = colnames(df[xvar]),
    y = colnames(df[classvar])
  )

# Histogram

ggplot(data = df) +
  geom_histogram(mapping = aes(x = df[, xvar])) +
  labs(
    title = paste0("Histogram of variable ", colnames(df[xvar])), x = colnames(df[xvar]),
    y = "Frequency"
  )

# -------------
# Tables

# Confidence limits for metric, Gaussian distributed variables

alpha <- 0.05

error <- qt(p = 1 - (alpha / 2), df = length(df[, xvar]) - 1) * (sd(df[, xvar]) / sqrt(length(df[, xvar])))
error

left <- mean(df[, xvar]) - error
right <- mean(df[, xvar]) + error

conf_table <- round(c(alpha, left, mean(df[, xvar]), right, error), digits = 2) # consider reevaluating digits

# SIMS

# variables to simulate: weight (-> bmi), chol, packyears, size, bili
# additional vars: alive -> y, n, c (alc, packyears, age, bili, size, dia)
# censored: size, bmi

prob_vec <- df_read$age * 0.0001 + df_read$size * 0.005 + df_read$bili * 0.00001 + df_read$alc * 0.0001 + df_read$packyears * 0.0001 + df_read$dia * 0.005

alive <- rbinom(n = 300, size = 1, prob = 1 - prob_vec)

alive[alive == 1] <- "y"
alive[alive == 0] <- "n"

length(alive[alive == "y"])

cens_vec <- df_read$size * 0.002 + (max(df_read$bmi) - df_read$bmi) * 0.001

cens <- rbinom(n = 300, size = 1, prob = 1 - cens_vec)
sum(cens)

alive[alive == "y" & cens == 0] <- "c"

length(alive[alive == "y"])

size_big <- read.csv("sims_size.csv", header = TRUE)
size_big <- size_big[, 2:13]

prob_matrix <- df_read$age * 0.0001 + size_big * 0.005 + bili_big * 0.00001 + df_read$alc * 0.0001 + df_read$packyears * 0.0001 + df_read$dia * 0.005
cens_matrix <- size_big * 0.002 + bmi_very_big * 0.0005 + bili_big * 0.0005

alive_matrix <- matrix(nrow = 300, ncol = 12)
alive_matrix[, 1] <- "y"

for (i in (2:12)) {
  for (j in (1:300)) {
    # print(alive_matrix[j,(i-1)]=="y")
    if (alive_matrix[j, (i - 1)] == "y") {
      if (rbinom(n = 1, size = 1, prob = prob_matrix[j, i]) == 1) {
        alive_matrix[j, i] <- "n"
      }
      else if (rbinom(n = 1, size = 1, prob = cens_matrix[j, i]) == 1) {
        alive_matrix[j, i] <- "c"
      }
      else {
        alive_matrix[j, i] <- "y"
      }
    }
    else {
      alive_matrix[j, i] <- alive_matrix[j, (i - 1)]
    }
  }
}


alive_final <- alive_matrix[, 12]
length(alive_final[alive_final == "n"])

colnames(alive_matrix) <- (1:12)

# write.csv(alive_matrix, file="sims_alive.csv")
# SIMS based on past values

# weight

test_weight <- df_read$weight[alive == "y"] + rnorm(n = length(df_read$weight[alive == "y"]), mean = -0.6, sd = 0.5)

test_weight_big <- matrix(nrow = 300, ncol = 12)
test_weight_big[1:300, ] <- df_read$weight

for (i in (1:11)) {
  test_weight_big[, i + 1] <- sim_weight(test_weight_big[, i])
}

sim_weight <- function(weight) {
  return(weight + rnorm(n = length(weight), mean = -0.6, sd = 0.5))
}

colnames(test_weight_big) <- (1:12)
test_weight_big <- round(test_weight_big, digits = 1)
# write.csv(test_weight_big, file="sims_weight.csv")

# size

test_size <- df_read$size + runif(n = 300, min = -0.01, max = +0.05)
test_size_big <- matrix(nrow = 300, ncol = 12)

sim_size <- function(size) {
  return(size + runif(n = 300, min = -0.01, max = +0.05))
}

test_size_big[, 1] <- df_read$size

for (i in (1:11)) {
  test_size_big[, i + 1] <- sim_size(test_size_big[, i])
}

colnames(test_size_big) <- (1:12)
test_size_big <- round(test_size_big, digits = 1)
# write.csv(test_size_big, "sims_size.csv")

# chol

test_chol_big <- matrix(nrow = 300, ncol = 12)

bmi <- df_read$weight / ((df_read$height)^2)

sim_chol <- function(chol) {
  return(chol + (((bmi - mean(bmi)) * 0.5) * rnorm(n = length(chol), mean = 0, sd = 0.2)))
}

test_chol_big[, 1] <- df_read$chol

for (i in (1:11)) {
  test_chol_big[, i + 1] <- sim_chol(test_chol_big[, i])
}

# BMI

height_big <- matrix(nrow = 300, ncol = 12)
height_big[, (1:12)] <- df_read$height

bmi_big <- round(test_weight_big / ((height_big / 100)^2), digits = 1)

df_weight <- read.csv("sims_weight.csv")

df_weight <- df_weight[, 2:13]

bmi_very_big <- round(df_weight / (df_read$height / 100)^2, digits = 1)

# write.csv(bmi_very_big, "sims_bmi.csv")

bili_big <- matrix(nrow = 300, ncol = 12)

bili_big[, 1] <- df_read$bili
for (i in (2:12)) {
  bili_big[, i] <- bili_big[, (i - 1)] + df_read$chol / 1000 + test_size_big[, i] / 20 + df_read$alc / 1000
}
bili_big <- round(bili_big, digits = 2)

colnames(bili_big) <- 1:12
# write.csv(bili_big, file="sims_bili.csv")

test_weight_big[alive_matrix != "y"] <- NA

# 0: not alive
# 1: alive
# -1: censored
set.seed(101010)
new_sex <- rbinom(n = 300, size = 1, prob = .5)
df_read$sex <- new_sex

# write.csv(df_read, row.names = FALSE, "liver_cancer.csv")
