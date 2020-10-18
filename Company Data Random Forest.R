#Random Forest

#Assignment


#About the data: 
#  Let's consider a Company dataset with around 10 variables and 400 records. 
#The attributes are as follows: 
#??? Sales -- Unit sales (in thousands) at each location
#??? Competitor Price -- Price charged by competitor at each location
#??? Income -- Community income level (in thousands of dollars)
#??? Advertising -- Local advertising budget for company at each location (in thousands of dollars)
#??? Population -- Population size in region (in thousands)
#??? Price -- Price company charges for car seats at each site
#??? Shelf Location at stores -- A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site
#??? Age -- Average age of the local population
#??? Education -- Education level at each location
#??? Urban -- A factor with levels No and Yes to indicate whether the store is in an urban or rural location
#??? US -- A factor with levels No and Yes to indicate whether the store is in the US or not

#  Problem Statement:
#  A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 
#Approach - A Random Forest can be built with target variable Sale (we will first convert it in categorical variable) & all other variable will be independent in the analysis.  

install.packages("readr")
library(readr)
install.packages("randomForest")
library(randomForest)
setwd('C://Users//Lenovo//Desktop//ExcelR//Assignments//Decision Trees')
getwd()
Company <- read.csv("Company_Data.csv")
library(readr)
View(Company)
install.packages('C50')
install.packages("ISLR")
library(ISLR)
hist(Company$Income, main = "company Income", col = c("red","blue", "green"))
plot(Company)

plot(Company$Sales,Company$Advertising)

Highsales <- ifelse(Company$Sales<=8,"No", "Yes")
head(Highsales)
Company <- data.frame(Company,Highsales)
View(Company)
Company <- Company[-1]
View(Company)
CompanyrandomForest <- randomForest(factor(Highsales) ~ ., data = Company, split=c("deviance", "gini"))

summary(CompanyrandomForest)
plot(CompanyrandomForest)

text(CompanyrandomForest, pretty = 1)
library(caret)
Training <- createDataPartition(Company$Highsales, p=0.70, list=F)
Train <- Company[Training,]
View(Train)
dim(Train)
Test <- Company[-Training,]
View(Test)
summary(Test)

Model_train <- randomForest(factor(Highsales)~., data = Train)
dim(Model_train)
summary(Model_train)

plot(Model_train);text(Model_train)
pred <- predict(Model_train, newdata=Test[-12], type = "class")
summary(pred)
head(pred)
dim(pred)
head(pred)
mean(pred==Test$Highsales)
library(gmodels)
CrossTable(pred,Test$Highsales)