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
