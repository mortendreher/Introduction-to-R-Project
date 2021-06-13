# Imputation method: KNN

# variables to be used: sex, age, bmi, size

df <- read.csv("C:/Users/Morten Dreher/Desktop/DSM/DSM5/R/Project/Introduction-to-R-Project/liver_cancer.csv", header=TRUE)

distances <- function(id) {
  distance_to_id <- numeric(length(df[,1]))
  distance_to_id[id] <- Inf
  
  for(i in (1:length(df[,1]))) {
    if(df$X[i] != id) {
      distance_to_id[i] = (abs(df$sex[i]-df$sex[id]) / (range(df$sex)[2]-range(df$sex)[1])) * 0.25
      distance_to_id[i] = distance_to_id[i] + abs(df$age[i]-df$age[id]) / (range(df$age)[2]-range(df$age)[1])
      distance_to_id[i] = distance_to_id[i] + (abs(df$bmi[i]-df$bmi[id]) / (range(df$bmi)[2]-range(df$bmi)[1])) * 1.5
      distance_to_id[i] = distance_to_id[i] + abs(df$size[i]-df$size[id]) / (range(df$size)[2]-range(df$size)[1])
    }

  }
  return(distance_to_id)
}

knn <- function(id, k) {
  distances <- distances(id)
  neighbour_ids <- numeric(k)
  neighbour_ids[1:k] <- -1
  neighbours_filled <- 0
  distances_sorted <- sort(distances, decreasing = FALSE)
  
  while(neighbours_filled < k) {
    nodes <- which(distances == min(distances))
    for (i in (1:length(nodes))) {
      neighbour_ids[(length(neighbour_ids[neighbour_ids!=-1])+1)] <- nodes[i]
    }
    neighbours_filled <- neighbours_filled + length(nodes)
    distances <- distances[distances != (min(distances))]
  }
  neighbour_ids <- neighbour_ids[1:k]
  return(neighbour_ids)
}

k <- c(3,5,7,9,11)

for(i in k) {
  print(paste("50 ",i, sd(df$chol[knn(50,i)]))) # min: k=5
  print(paste("72 ",i, sd(df$chol[knn(72,i)]))) # min: k=3
  print(paste("182 ",i, sd(df$chol[knn(182,i)]))) # min: k=9
  print("---------")
}

# choose k=3

sd(df$chol[knn(50,k)])
mean(df$chol[knn(72,k)])
mean(df$chol[knn(182,k)])

knn(50, 7)
sort(distances(50))
distances(50)[53]
distances(50)[114]
distances(50)[130]
distances(50)[173]
distances(50)[256]

sort(distances(72)) 
knn(72,5)

sort(distances(182))
knn(182, 5)
distances(182)[30]

# -------------------- 
# PLOTS 
# ------------
# Scatterplot
library(ggplot2)
library(dplyr)

colnames(df) <- c("ID", "Sex", "Age", "Height", "Weight", "BMI", "Diet", 
                  "Cholesterol", "Smoker", "Cigs per day", "Packyears", "Alcohol", "Tumour size", "Bilirubin", "HBV", "HCV","Diabetes")

xvar <- 4
yvar <- 14
classvar <- 7

colnames(df[xvar])
colnames(df[yvar])
View(df)

# example of a dynamic scatter plot. user input will determine x, y and class variables
# reactive: colour TRUE / FALSE

plot_title <- paste0("Variables ", colnames(df[xvar]) , " and ", colnames(df[yvar]))
plot_title
ggplot(data=df, mapping=aes(x=df[,xvar], y=df[,yvar])) + geom_point(mapping=aes(colour=df[,classvar])) + 
  labs(title=paste0("Variables ", colnames(df[xvar]) , " and ", colnames(df[yvar])), x=colnames(df[xvar]), 
       y=colnames(df[yvar]), colour=colnames(df[classvar]))

# Boxplot 

# consider adding character variable for sex

classvar <- 2

ggplot(data=df) + geom_boxplot(mapping = aes(x = df[,xvar], y=df[,classvar], group=df[,classvar])) + 
  labs(title=paste0("Variables ", colnames(df[xvar]) , " and ", colnames(df[classvar])), x=colnames(df[xvar]), 
       y=colnames(df[classvar]))

# Histogram 

ggplot(data=df) + geom_histogram(mapping = aes(x=df[,xvar])) +
  labs(title=paste0("Histogram of variable ", colnames(df[xvar])), x=colnames(df[xvar]), 
       y="Frequency")

# -------------
# Tables       

# Summaries for metric variables

summary_table<- t(as.numeric(summary(df[,xvar])))
knitr::kable(summary_table, col.names = c("Min", "Q1", "Median", "Mean", "Q3", "Max"))

# Risks for two dichotomous variables

dvar1 <- 2
dvar2 <- 17

knitr::kable(table(df[,dvar1], df[,dvar2]))

# Confidence limits for metric, Gaussian distributed variables

alpha <- 0.05 

error <- qt(p=1-(alpha/2), df=length(df[,xvar])-1)*(sd(df[,xvar])/sqrt(length(df[,xvar])))
error

left <- mean(df[,xvar]) - error
right <- mean(df[,xvar]) + error

conf_table <- round(c(alpha, left, mean(df[,xvar]), right, error), digits=2) # consider reevaluating digits

knitr::kable(t(conf_table), col.names = c("Alpha", "Lower limit", "Mean", "Upper limit", "Standard error"))
