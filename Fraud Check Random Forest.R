#Use Random Forest to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"


install.packages("readr")
library(readr)
setwd('C://Users//Lenovo//Desktop//ExcelR//Assignments//Random Forests')
getwd()
Fraud <- read.csv("Fraud_check.csv")
View(Fraud)
install.packages("ISLR")
library(ISLR)
install.packages("randomForest")
library(randomForest)
hist(Fraud$Taxable.Income)
FaadMagic <- ifelse(Fraud$Taxable.Income<=30000,"Risky","Good")
head(FaadMagic)

Fraud <- data.frame(Fraud,FaadMagic)
View(Fraud)
Fraud <- Fraud[-3]
View(Fraud)
FraudrandomForest <- randomForest(factor(FaadMagic)~. , data = Fraud, split=c("deviance", "gini"))
is.na(FraudrandomForest)
sum(is.na(FraudrandomForest))
str(FraudrandomForest)
summary(FraudrandomForest)
library(caret)
Training <- createDataPartition(Fraud$FaadMagic, p=0.50, list=F)

Train <- Fraud[Training,]
table(Train$FaadMagic)

Test <- Fraud[-Training,]
table(Test$FaadMagic)

Model_train <- randomForest(factor(FaadMagic)~., data = Train)
dim(Model_train)
summary(Model_train)


pred <- predict(Model_train, newdata=Test[-7], type = "class")
summary(pred)


dim(pred)

mean(pred==Test$FaadMagic)
library(gmodels)
CrossTable(pred,Test$FaadMagic)
confusionMatrix(pred,factor(Test$FaadMagic))
# We find the model is overfit in pred object there is only "Good" value.
#To over come this we will apply begging methond
acc <- NULL
for (i in 1:100) 
{ print(i)
  Training <- createDataPartition(factor(Fraud$FaadMagic), p=0.50, list=F)
  Train <- Fraud[Training,]
  Test <- Fraud[-Training,]
  
  Model_train <- randomForest(factor(FaadMagic)~., data = Train)
  pred <- predict(Model_train, newdata=Test, type = "class")
  
  acc <- c(acc, mean(pred==Test$FaadMagic))
  
}
mean(acc)
CrossTable(pred,Test$FaadMagic)