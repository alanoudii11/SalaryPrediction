install.packages("outliers")
install.packages("scatterplot3d") 
install.packages("dplyr")


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
OutcapitalG = outlier(dataset$capital-gain, logical =TRUE)
sum(OutcapitalG)
Find_outlier = which(OutcapitalG ==TRUE, arr.ind = TRUE)
OutcapitalG
###
OutcapitalL = outlier(dataset$capital-loss, logical =TRUE)
sum(OutcapitalL)
Find_outlier = which(OutcapitalL ==TRUE, arr.ind = TRUE)
OutcapitalL


#Remove outlier
dataset= dataset[-Find_outlier,]
print(dataset)


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
hist(dataset$age)
hist(dataset$num_salary)
hist(dataset$fnlwgt)
hist(dataset$`education-num`)
hist(dataset$`capital-gain`)
hist(dataset$`capital-loss`)
hist(dataset$`hours-per-week`)

#  bar plots 
barplot(table(dataset$workclass))
barplot(table(dataset$education))
barplot(table(dataset$`marital-status`))
barplot(table(dataset$occupation))
barplot(table(dataset$relationship))
barplot(table(dataset$race))
barplot(table(dataset$sex))
barplot(table(dataset$`native-country`))
barplot(table(dataset$salary))

# box plot 
boxplot(age~ salary, data = dataset)
boxplot(`hours-per-week`~ salary, data = dataset)

#scatter plot

scatterplot3d(dataset$num_salary,dataset$age, dataset$`hours-per-week`)


#pie chart for the salary attribute

tab <- dataset$salary %>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100 
txt <- paste0(names(tab), '\n', precentages, '%') 
pie(tab, labels=txt) 




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
dataset$capital-gain<-normalize(dataWithoutNormalization$capital-gain)
print(dataset)

#Call Z_normalize funcrtion 
dataset$capital-gain<-Z_normalize(dataWithoutNormalization$capital-gain)
print(dataset)

######################################

#Call normalize funcrtion 
dataset$capital-loss<-normalize(dataWithoutNormalization$capital-loss)
print(dataset)

#Call Z_normalize funcrtion 
dataset$capital-loss<-Z_normalize(dataWithoutNormalization$capital-loss)
print(dataset)










