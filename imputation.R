# Imputation method: KNN

# variables to be used: sex, age, bmi, size

df_read <- read.csv("C:/Users//Desktop/DSM/DSM5/R/Project/Introduction-to-R-Project/liver_cancer.csv", header=TRUE)
df[c(53,114,130,173),]

if(Sys.info()[["user"]] == 'Morten Dreher'){
  df_read <- read.csv("C:/Users//Desktop/DSM/DSM5/R/Project/Introduction-to-R-Project/liver_cancer.csv", header=TRUE)
} else if(Sys.info()[["user"]] == 'Lars Andersen'){
  df_read <- read.csv("C:/Users/Lars Andersen/Desktop/DSM/Semester 5/R shiny/project/Introduction-to-R-Project/liver_cancer.csv", header=TRUE)
} else if(Sys.info()[["user"]] == 'Melita Coneva'){
  df_read <- read.csv("C:/Users/Coneva/Desktop/ShinyApp/Introduction-to-R-Project/liver_cancer.csv", header=TRUE)
}

range(df$alc)
summary(df$alc)
View(df_read)
range(df_read[,3])
range(df$bili)
library(dplyr)

length(df_read[,1])
df_read[2,40]
typeof(df_read[40,3])
df_read[40,3]

distances <- function(df, id, vars, weights=rep.int(1,length(vars))) {
  distance_to_id <- numeric(length(df[,1]))
  distance_to_id[id] <- Inf # ensures that node itself will not be returned
  
  for(v in vars) {
    for(i in (1:length(distance_to_id))) {
      if(i != id) {
        rng <- range(df[,v])
        distance_to_id[i] = distance_to_id[i] + (abs(df[i,v]-df[id,v])/(rng[2]-rng[1]))*weights[which(vars==v)]
      }
    }
  }
  return(distance_to_id)
}

knn <- function(df, id, vars, k, weights) {
  distances_to_id <- distances(df, id, vars, weights)
  df_with_distances <- cbind(df, distances_to_id)
  df_with_distances <- arrange(df_with_distances, df_with_distances$distances_to_id)
  return(df_with_distances$X[1:k])
}

var <- c(2,3,6,13)
w <- c(0.25,1,1.5,1)
mean(df_read$chol[knn(df_read,id=50, vars=var, k=3, weights=w)])
mean(df_read$chol[knn(df_read,id=72, vars=var, k=3, weights=w)])
mean(df_read$chol[knn(df_read,id=182, vars=var, k=3, weights=w)])

distances(df_read, id=50,vars=var, weights=w)

length(distances(df_read, id=50))
meme <- cbind(df_read, distances(df_read, 50))
View(meme)


k <- c(3,5,7,9,11)

for(i in k) {
  print(paste("50 ",i, sd(df$chol[knn(50,i)]))) # min: k=5
  print(paste("72 ",i, sd(df$chol[knn(72,i)]))) # min: k=3
  print(paste("182 ",i, sd(df$chol[knn(182,i)]))) # min: k=9
  print("---------")
}

# choose k=3

sd(df$chol[knn(df, 50,k)])
mean(df$chol[knn(df, 72,k)])
mean(df$chol[knn(df, 182,k)])

table_3 <- rbind(c(50, df$chol[knn(df,50,3)], mean(df$chol[knn(df,50,3)]), sd(df$chol[knn(df,50,3)])),
                 c(72, df$chol[knn(df,72,3)], mean(df$chol[knn(df,72,3)]), sd(df$chol[knn(df,72,3)])),
                 c(182, df$chol[knn(df,182,3)], mean(df$chol[knn(df,182,3)]), sd(df$chol[knn(df,182,3)]))
                 )

knitr::kable(round(table_3, digits=2), col.names=c("ID of NA", "Neighbour 1", "Neighbour 2", "Neighbour 3", "Mean", "Sd"))

table_5 <- rbind(c(50, df$chol[knn(df,50,5)], mean(df$chol[knn(df,50,5)]), sd(df$chol[knn(df,50,5)])),
                 c(72, df$chol[knn(df,72,5)], mean(df$chol[knn(df,72,5)]), sd(df$chol[knn(df,72,5)])),
                 c(182, df$chol[knn(df,182,5)], mean(df$chol[knn(df,182,5)]), sd(df$chol[knn(df,182,5)]))
)

knitr::kable(table_5, col.names=c("ID of NA", "Neighbour 1", "Neighbour 2", "Neighbour 3","Neighbour 4","Neighbour 5", "Mean", "Sd"))

knn(df,50, 7)
sort(distances(df,50))
distances(df,50)[53]
distances(df,50)[114]
distances(df,50)[130]
distances(df,50)[173]
distances(df,50)[256]

View(df)

sort(distances(df,72)) 
knn(df,72,5)

sort(distances(df,182))
knn(df,182, 5)
distances(df,182)[30]

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

summary_table<- t(as.numeric(summary(df_read[,xvar])))
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
