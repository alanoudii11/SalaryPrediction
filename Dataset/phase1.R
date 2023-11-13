install.packages("outliers")
install.packages("scatterplot3d") 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("colorspace")


library(ggplot2)
library(outliers)
library(scatterplot3d)
library(dplyr)
library(readr)
dataset <- read_csv("GitHub/SalaryPrediction/Dataset/salary.csv")




# Sample of raw dataset
View(dataset)
summary(dataset)

# Missing values
sum(is.na(dataset))
# we noticed that there is a question mark in some values so we are going to clean them (deleting the row)
dataset <- dataset[!apply(dataset == "?", 1, any), ]


# Create a new column called num_salary to transform the binary attribute into numrical to use it better as it it the class label
dataset$num_salary <- ifelse(dataset$salary == ">50K", 1, 0)
View(dataset)



#removing outlier

OutAge = outlier(dataset$age, logical =TRUE)
sum(OutAge)
Find_outlier = which(OutAge ==TRUE, arr.ind = TRUE)
OutAge
Find_outlier
####
Outfnlwgt = outlier(dataset$fnlwgt, logical =TRUE)
sum(Outfnlwgt)
Find_outlier = which(Outfnlwgt ==TRUE, arr.ind = TRUE)
Outfnlwgt
###
Outeducation = outlier(dataset$`education-num`, logical =TRUE)
sum(Outeducation)
Find_outlier = which(Outeducation ==TRUE, arr.ind = TRUE)
Outeducation
##

Outhours = outlier(dataset$`hours-per-week`, logical =TRUE)
sum(Outhours)
Find_outlier = which(Outhours ==TRUE, arr.ind = TRUE)
Outhours
###
OutcapitalG = outlier(dataset$`capital-gain`, logical =TRUE)
sum(OutcapitalG)
Find_outlier = which(OutcapitalG ==TRUE, arr.ind = TRUE)
OutcapitalG
###
OutcapitalL = outlier(dataset$`capital-loss`, logical =TRUE)
sum(OutcapitalL)
Find_outlier = which(OutcapitalL ==TRUE, arr.ind = TRUE)
OutcapitalL


#Remove outlier
dataset= dataset[-Find_outlier,]



# Create a bar plot of the occupation variable
ggplot(dataset, aes(x = `native-country`)) +
  geom_bar()

# Create a density plot for the age variable
ggplot(dataset, aes(x = age)) +
  geom_density()

# Create a scatter plot of the age and salary variables
ggplot(dataset, aes(x = age, y = salary)) +
  geom_point()
# Create a boxplot of the salary variable
ggplot(dataset, aes(x = "", y = salary)) +
  geom_boxplot()

# Data summary + variance 

summary(dataset$age)
summary(dataset$`hours-per-week`)
summary(dataset$education)
summary(dataset$occupation)
summary(dataset$sex)
summary(dataset$salary)

var(dataset$age)
var(dataset$`hours-per-week`)
# Discretize the hours-per-week into three bins
dataset$hours_per_week_binned <- cut(dataset$`hours-per-week`, breaks = c(0, 30, 40, Inf), labels = c("Low", "Medium", "High"))
View(dataset)




dataset$workclass <-  as.integer(factor(dataset$workclass))
dataset$`marital-status` <-  as.integer(factor(dataset$`marital-status`))
dataset$occupation <-  as.integer(factor(dataset$occupation))
dataset$relationship <-  as.integer(factor(dataset$workclass))
dataset$race <-  as.integer(factor(dataset$race))
dataset$sex <-  as.integer(factor(dataset$sex))
dataset$`native-country` <-  as.integer(factor(dataset$`native-country`))
dataset$education <-  as.integer(factor(dataset$education))
View(dataset)

# histograms


# Histogram of the age variable
ggplot(dataset, aes(x = age)) +
  geom_histogram()

# Histogram of the num_salary variable where the 1 represents salary >=50k and 0 represents salary <50k
ggplot(dataset, aes(x = num_salary)) +
  geom_histogram()

# Histogram of the hours-per-week variable
ggplot(dataset, aes(x = `hours-per-week`)) +
  geom_histogram()

# Histogram of the fnlwgt variable
ggplot(dataset, aes(x = fnlwgt)) +
  geom_histogram()





#  bar plots 


# Bar plot of the workclass variable
ggplot(dataset, aes(x = workclass)) + geom_bar()

# Bar plot of the education variable
ggplot(dataset, aes(x = education)) + geom_bar()

# Bar plot of the marital-status variable
ggplot(dataset, aes(x = `marital-status`)) + geom_bar()

# Bar plot of the occupation variable
ggplot(dataset, aes(x = occupation)) + geom_bar()

# Bar plot of the relationship variable
ggplot(dataset, aes(x = relationship)) + geom_bar()

# Bar plot of the sex variable
ggplot(dataset, aes(x = sex)) +geom_bar()

# Bar plot of the native-country variable
ggplot(dataset, aes(x = `native-country`)) + geom_bar()

# Bar plot of the salary variable
ggplot(dataset, aes(x = salary)) + geom_bar()


# box plot 



# Box plot of hours-per-week vs. salary
ggplot(dataset, aes(x = salary, y = `hours-per-week`)) +
  geom_boxplot() +
  labs(title = "Box Plot of Salary and Hours per Week", x = "Salary", y = "Hours per Week")


ggplot(dataset, aes(x = salary, y = age)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age and Salary", x = "salary", y = "age")




#scatter plot

scatterplot3d(dataset$num_salary,dataset$age, dataset$`hours-per-week`)





#pie chart for the salary attribute

tab <- dataset$salary %>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100 
txt <- paste0(names(tab), '\n', precentages, '%') 
pie(tab, labels=txt) 


# Create a density plot for the age variable
ggplot(dataset, aes(x = age)) +
  geom_density()



dataWithoutNormalization <- dataset
print(dataWithoutNormalization)
#Define function normalize().
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
#Define function Z_normalize().
Z_normalize <- function(x) {return ((x - mean(x)) / sd(x))}

#Call normalize funcrtion 
dataset$age<-normalize(dataWithoutNormalization$age)
print(dataset)

#Call Z_normalize funcrtion 
dataset$age<-Z_normalize(dataWithoutNormalization$age)
print(dataset)

######################################

#Call normalize funcrtion 
dataset$fnlwgt<-normalize(dataWithoutNormalization$fnlwgt)
print(dataset)

#Call Z_normalize funcrtion 
dataset$fnlwgt<-Z_normalize(dataWithoutNormalization$fnlwgt)
print(dataset)

######################################

#Call normalize funcrtion 
dataset$`education-num`<-normalize(dataWithoutNormalization$`education-num`)
print(dataset)

#Call Z_normalize funcrtion 
dataset$`education-num`<-Z_normalize(dataWithoutNormalization$`education-num`)
print(dataset)

######################################



#Call normalize funcrtion 
dataset$`hours-per-week`<-normalize(dataWithoutNormalization$`hours-per-week`)
print(dataset)

#Call Z_normalize funcrtion 
dataset$`hours-per-week`<-Z_normalize(dataWithoutNormalization$`hours-per-week`)
print(dataset)

######################################

#Call normalize funcrtion 
dataset$`capital-gain`<-normalize(dataWithoutNormalization$`capital-gain`)
print(dataset)

#Call Z_normalize funcrtion 
dataset$`capital-gain`<-Z_normalize(dataWithoutNormalization$`capital-gain`)
print(dataset)

######################################

#Call normalize funcrtion 
dataset$`capital-loss`<-normalize(dataWithoutNormalization$`capital-loss`)
print(dataset)

#Call Z_normalize funcrtion 
dataset$`capital-loss`<-Z_normalize(dataWithoutNormalization$`capital-loss`)
print(dataset)


dataset<- subset(dataset, select = -`capital-gain`)
dataset<- subset(dataset, select = -`capital-loss`)


### PHASE 2 ###


dataset <- subset(dataset, select = -salary)

head(dataset)

str(dataset)

# Generate some example data
set.seed(100)
X <- matrix(rnorm(100), ncol = 2)


# Set the sizes of K
k_values <- c(3, 5, 7)

clusters <- list()
for (k in k_values) {
  clusters[[k]] <- kmeans(dataset, k)
}

# Calculate the silhouette coefficient
silhouette_coefficients <- sapply(clusters, function(x) silhouette(x$cluster))

# Calculate the total within-cluster sum of squares
twss <- sapply(clusters, function(x) sum(x$distances^2))

# Calculate the BCubed precision and recall
b_cubed <- sapply(clusters, function(x) {
  # Get the true cluster labels
  true_labels <- table(dataset)
  
  # Get the predicted cluster labels
  predicted_labels <- x$cluster
  
  # Calculate the BCubed precision and recall
  b_cubed <- precision_recall_f1(true_labels, predicted_labels, average = "weighted")
  
  return(b_cubed)
})
install.packages("dummies")

install.packages("tidyverse")
library(tidyverse)
library(dummies)
# Select the columns you want to one-hot encode
cols_to_encode <- c("workclass", "education", "marital-status", "occupation", "relationship", "race", "native-country")

# One-hot encode categorical variables using the 'dummies' library


data_encoded <- dummy.data.frame(data, cols = cols_to_encode)

# Remove the original categorical columns
data_encoded <- select(data_encoded, -cols_to_encode)

# Scale the numerical variables to have zero mean and unit variance
data_scaled <- scale(data_encoded)

# Convert categorical variables to numerical using one-hot encoding (for workclass, education, marital-status, occupation, relationship, race, and native-country)
dataset <- dataset %>%
  select(-salary) %>%
  select(-c("workclass", "education", "marital-status", "occupation", "relationship", "race", "native-country")) %>%
  mutate(across(where(is.character), as.factor)) %>%
  model.matrix(~., dataset = .)

# Scale the numerical variables to have zero mean and unit variance
data_scaled <- scale(dataset)

wcss <- numeric(10)  # Initialize an empty vector to store WCSS values
for (i in 1:10) {
  kmeans_model <- kmeans(data_scaled, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plot the WCSS values
plot(1:10, wcss, type = "b", xlab = "Number of Clusters (K)", ylab = "WCSS")

# Choose the optimal K (e.g., K=3)
k_optimal <- 3

# Fit K-means clustering model
kmeans_model <- kmeans(data_scaled, centers = k_optimal)

# Add cluster labels to the original dataset
data$cluster_label <- kmeans_model$cluster





# Perform Hierarchical Clustering
hc <- hclust(dist(X))

# Plot the dendrogram
plot(hc)



# Delete the salary column
dataset <- dataset$salary
View(dataset)
# Set the sizes of K
k_values <- c(3, 5, 7)

# Apply K-means clustering
clusters <- list()
for (k in k_values) {
  clusters[[k]] <- kmeans(dataset, k)
}




















install.packages("caret")
install.packages("lattice")




### PHASE 2 ###
data <- read_csv("GitHub/SalaryPrediction/Dataset/salary.csv")
# Missing values
sum(is.na(data))
data <- na.omit(data)
# we noticed that there is a question mark in some values so we are going to clean them (deleting the row)
data <- data[!apply(data == "?", 1, any), ]
data("data")
data <- scale(data)
View(data)

#removing outlier

OutAge = outlier(data$age, logical =TRUE)
sum(OutAge)
Find_outlier = which(OutAge ==TRUE, arr.ind = TRUE)
OutAge
Find_outlier
####
Outfnlwgt = outlier(data$fnlwgt, logical =TRUE)
sum(Outfnlwgt)
Find_outlier = which(Outfnlwgt ==TRUE, arr.ind = TRUE)
Outfnlwgt
###
Outeducation = outlier(data$`education-num`, logical =TRUE)
sum(Outeducation)
Find_outlier = which(Outeducation ==TRUE, arr.ind = TRUE)
Outeducation
##

Outhours = outlier(data$`hours-per-week`, logical =TRUE)
sum(Outhours)
Find_outlier = which(Outhours ==TRUE, arr.ind = TRUE)
Outhours
###
OutcapitalG = outlier(data$`capital-gain`, logical =TRUE)
sum(OutcapitalG)
Find_outlier = which(OutcapitalG ==TRUE, arr.ind = TRUE)
OutcapitalG
###
OutcapitalL = outlier(data$`capital-loss`, logical =TRUE)
sum(OutcapitalL)
Find_outlier = which(OutcapitalL ==TRUE, arr.ind = TRUE)
OutcapitalL
# Convert columns to numeric
data <- mutate_all(data, as.numeric)

#Remove outlier
data= data[-Find_outlier,]


library(caret)

# Numeric feature scaling
numeric_cols <- c("age", "fnlwgt", "education-num", "capital-gain", "capital-loss", "hours-per-week")
data[, numeric_cols] <- scale(data[, numeric_cols])

# Categorical feature encoding
categorical_cols <- c("workclass", "education", "marital-status", "occupation", "relationship", "race", "native-country")
encoded_data <- data.frame(model.matrix(~.-1, data = data[categorical_cols]))

# Combining the processed data
processed_data <- cbind(data[, -which(names(data) %in% c(numeric_cols, categorical_cols))], encoded_data)







# Perform K-means clustering for different values of K
k <- c(3, 4, 5)  # Different values of K
results <- list()  # To store the clustering results

for (i in k) {
  # Apply K-means clustering
  cluster <- kmeans(processed_data, centers = i)
  results[[as.character(i)]] <- cluster
}

# Compute silhouette values
silhouette_vals <- list()

for (i in k) {
  cluster <- results[[as.character(i)]]
  silhouette_vals[[as.character(i)]] <- silhouette(cluster$cluster, dist(processed_data))
}

# Access silhouette values for K = 3
silhouette_vals$`3`








install.packages('cluster')
library(cluster)
install.packages('factoextra')
library(factoextra)


install.packages("fviz")
library(fviz)

install.packages('DPBBM')
  library(DPBBM)





### PHASE 2 ###
dataset <- read_csv("GitHub/SalaryPrediction/Dataset/salary.csv")
dataset <- subset(dataset, select = -salary)
View(dataset)
# Missing values
sum(is.na(dataset))
dataset <- na.omit(dataset)
# we noticed that there is a question mark in some values so we are going to clean them (deleting the row)
dataset <- dataset[!apply(dataset == "?", 1, any), ]
View(dataset)

dataset$workclass <-  as.integer(factor(dataset$workclass))
dataset$`marital-status` <-  as.integer(factor(dataset$`marital-status`))
dataset$occupation <-  as.integer(factor(dataset$occupation))
dataset$relationship <-  as.integer(factor(dataset$workclass))
dataset$race <-  as.integer(factor(dataset$race))
dataset$sex <-  as.integer(factor(dataset$sex))
dataset$`native-country` <-  as.integer(factor(dataset$`native-country`))
dataset$education <-  as.integer(factor(dataset$education))
View(dataset)

dataset=scale(dataset) #scaling and centering of data set objects



#clustering using K-means: 

  set.seed(8789) #set a seed for random number generation to make the results reproducible


#• K-means clustering to find 2 clusters:
  kmeans.2=kmeans(dataset,2)
  kmeans.2
  fviz_cluster (kmeans.2, data=dataset)
  avg_sil=silhouette (kmeans.2$cluster, dist(dataset))
  fviz_silhouette(avg_sil)
  
  
#• K-means clustering to find 3 clusters:
  
  kmeans.3=kmeans(dataset,3)
  kmeans.3
  fviz_cluster (kmeans.3, data=dataset)
  avg_sil=silhouette (kmeans.3$cluster, dist(dataset))
  fviz_silhouette(avg_sil)
  
  
#• K-means clustering to find 4 clusters:
  kmeans.4=kmeans(dataset,4)
  kmeans.4
  fviz_cluster (kmeans.4, data=dataset)
  avg_sil=silhouette (kmeans.4$cluster, dist(dataset))
  fviz_silhouette(avg_sil)
  
  
  
  
  
  install.packages('fpc')
  library(fpc)
  silhouette.2 <- cluster.stats(dist(dataset), kmeans.2$cluster)$silinfo$avg.width
  silhouette.3 <- cluster.stats(dist(dataset), kmeans.3$cluster)$silinfo$avg.width
  silhouette.4 <- cluster.stats(dist(dataset), kmeans.4$cluster)$silinfo$avg.width
  
  silhouette.2 <- silhouette(as.integer(kmeans.2$cluster), dist(dataset))$avg
  silhouette.3 <- silhouette(dataset, kmeans.3$cluster)$avg
  silhouette.4 <- silhouette(dataset, kmeans.4$cluster)$avg
  
  bclust.2_precision <- BCubed(dataset, kmeans.2$cluster)$measures$precision
  bclust.3_precision <- BCubed(dataset, kmeans.3$cluster)$measures$precision
  bclust.4_precision <- BCubed(dataset, kmeans.4$cluster)$measures$precision
  
  bclust.2_recall <- BCubed(dataset, kmeans.2$cluster)$measures$recall
  bclust.3_recall <- BCubed(dataset, kmeans.3$cluster)$measures$recall
  bclust.4_recall <- BCubed(dataset, kmeans.4$cluster)$measures$recall
  
  # Define the silhouette_score function
  silhouette_score <- function(data, k) {
    km <- kmeans(data, centers = k, nstart = 25)
    ss <- silhouette(km$cluster, dist(data))
    sil <- mean(ss[, 3])
    return(sil)
  }
  
  # Initialize variables to store the best K and best silhouette score
  best_k <- 2
  best_silhouette <- silhouette_score(dataset, best_k)
  
  # Loop through different K values and find the best one
  for (k in 2:10) {
    silhouette <- silhouette_score(dataset, k)
    if (silhouette > best_silhouette) {
      best_k <- k
      best_silhouette <- silhouette
    }
  }
  
  cat("Best number of clusters:", best_k, "\n")
  cat("Best silhouette score:", best_silhouette, "\n")
  
  

  
  # Evaluate the clustering results using the Silhouette coefficient
  silhouette.2 <- silhouette(dataset, kmeans.2$cluster)$avg
  silhouette.3 <- silhouette(dataset, kmeans.3$cluster)$avg
  silhouette.4 <- silhouette(dataset, kmeans.4$cluster)$avg
  
  
  # Evaluate the clustering results using the total within-cluster sum of squares
  wss.2 <- sum(kmeans.2$withinss)
  wss.3 <- sum(kmeans.3$withinss)
  wss.4 <- sum(kmeans.4$withinss)
  
  # Evaluate the clustering results using the BCubed precision and recall
  
  bclust.2 <- BCubed(dataset, kmeans.2$cluster)$measures$precision
  bclust.3 <- BCubed(dataset, kmeans.3$cluster)$measures$precision
  bclust.4 <- BCubed(dataset, kmeans.4$cluster)$measures$precision
  
  bclust.2 <- BCubed(dataset, kmeans.2$cluster)$measures$recall
  bclust.3 <- BCubed(dataset, kmeans.3$cluster)$measures$recall
  bclust.4 <- BCubed(dataset, kmeans.4$cluster)$measures$recall
  
  
  # Create a data frame to store evaluation results
  evaluation_results <- data.frame(
    K = c(2, 3, 4),
    Silhouette = c(silhouette.2, silhouette.3, silhouette.4),
    WSS = c(wss.2, wss.3, wss.4),
    BCubed_Precision = c(bclust.2_precision, bclust.3_precision, bclust.4_precision),
    BCubed_Recall = c(bclust.2_recall, bclust.3_recall, bclust.4_recall)
  )
  
  # View the evaluation results
  print(evaluation_results)
  
  
  #• Clustering results: #this code repeated for each K-means
  kmeans.result #print the clustering results
  kmeans.result
  fviz_cluster (kmeans.result, data=dataset)
  avg_sil=silhouette (kmeans.result$cluster, dist(dataset))
  fviz_silhouette(avg_sil)
  remove(kmeans.result)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#• Clusters plot: #this code repeated for each K-means
  fviz_cluster (kmeans.result, data=dataset)

#• average for each cluster #this code repeated for each K-means
avg_sil=silhouette (kmeans.result$cluster, dist(dataset))
fviz_silhouette(avg_sil)
remove(kmeans.result) #remove the variable for each k-mean



#Extract cluster information:
  avg_sil=silhouette (kmeans.result$cluster, dist(dataset))
fviz_silhouette(avg_sil)

#• Determining the optimal number of clusters:
  fviz_nbclust (dataset, kmeans, method="silhouette") + labs (subtitle="Silhouette method")
















# Step 4: Apply K-means clustering
set.seed(123)  # Set a random seed for reproducibility
k <- 3  # Choose the number of clusters
kmeans.result <- kmeans(dataset, centers = k)
















install.packages("dplyr")
install.packages("caret")
install.packages("fclust")
library(dplyr)
library(caret)
library(fclust)




# Encode categorical variables using one-hot encoding
# Convert categorical variables into factors
dataset$workclass <- as.factor(dataset$workclass)
dataset$education <- as.factor(dataset$education)
dataset$`marital-status` <- as.factor(dataset$`marital-status`)
dataset$occupation <- as.factor(dataset$occupation)
dataset$relationship <- as.factor(dataset$relationship)
dataset$race <- as.factor(dataset$race)
dataset$sex <- as.factor(data$sex)
dataset$`native-country` <- as.factor(dataset$`native-country`)
View(dataset)

# One-hot encode categorical variables
onehot_data <- model.matrix(dataset[, -1], dataset$income)

# Apply K-means clustering to the one-hot encoded data
# Set the number of clusters (K) to 3, 5, and 7
k_values <- c(3, 5, 7)

for (k in k_values) {
  # Perform K-means clustering
  kmeans_fit <- kmeans(onehot_data, clusters = k)
  
  # Evaluate clustering performance using silhouette coefficient, total within-cluster sum of square, BCubed precision and recall
  sil_score <- silhouette(onehot_data, kmeans_fit$cluster)
  wcss <- sum(kmeans_fit$withinss)
  b3_precision <- cluster::b3_precision(as.factor(kmeans_fit$cluster), data$income)
  b3_recall <- cluster::b3_recall(as.factor(kmeans_fit$cluster), data$income)
  
  # Visualize clustering results using scatter plots
  plot(kmeans_fit$cluster, col = kmeans_fit$cluster)
  
  # Print evaluation metrics
  print(paste("Silhouette coefficient for K =", k, ":", mean(sil_score)))
  print(paste("Total within-cluster sum of square for K =", k, ":", wcss))
  print(paste("BCubed precision for K =", k, ":", b3_precision))
  print(paste("BCubed recall for K =", k, ":", b3_recall))
}


install.packages('cluster.stats')



# Silhouette Coefficient
library(cluster)
silhouette_scores <- vector("double", 4)  # Create an empty vector to store Silhouette coefficients

for (k in 2:5) {
  kmeans_model <- kmeans(dataset, k)
  silhouette_scores[k - 1] <- mean(silhouette(kmeans_model$cluster, dist(dataset)))
}

# Find the best k based on Silhouette
best_k_silhouette <- which.max(silhouette_scores) + 1
cat("Best k based on Silhouette:", best_k_silhouette, "\n")
cat("Silhouette Scores:", silhouette_scores, "\n")

# Total Within-Cluster Sum of Squares (WCSS)
wcss <- vector("double", 4)  # Create an empty vector to store WCSS

for (k in 2:5) {
  kmeans_model <- kmeans(dataset, k)
  wcss[k - 1] <- kmeans_model$tot.withinss
}

# Find the best k based on the elbow method
best_k_wcss <- which.min(wcss) + 1
cat("Best k based on WCSS (Elbow Method):", best_k_wcss, "\n")
cat("WCSS Values:", wcss, "\n")

# BCubed Precision and Recall (assuming you have a ground truth dataset, e.g., true_labels)

library(fpc)
true_labels <- c(0,1 )  # Replace with your actual ground truth labels
predicted_clusters <- kmeans(dataset, best_k_silhouette)$cluster

bcubed_result <- cluster.stats(predicted_clusters, true_labels)
cat("BCubed Precision:", bcubed_result$Bcubed$precision, "\n")
cat("BCubed Recall:", bcubed_result$Bcubed$recall, "\n")











## clustring










set.seed(8990) #set a seed for random number generation to make the results reproducible
dataset=scale(dataset)
