#Add Libraries
library("e1071")
library("Metrics")
library(RODBC)
library("rpart")
library(randomForest)

#Set Globals
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\BloodDonations")

#Read Files
Train = read.csv("train.csv", header = TRUE)
Test = read.csv("test.csv", header = TRUE)
attach(Train)

#Train Data
model.rpart = rpart(Made.Donation.in.March.2007~., data=Train[,-1], method="class")

Predicted.Train2 = cbind(Train, predicted = predict(model.rpart, Train[,-1], type="class"))
a = cbind(o = Predicted.Train2$Made.Donation.in.March.2007, p = Predicted.Train2$predicted)




Predicted.Train.svm = cbind(Train, predict = as.integer(predict(model.svm, Train[,-1], interval="response", type="prob")))
Predicted.Test.svm = cbind(Test, predict = as.integer(predict(model.svm, Test[,-1], interval="predict")))
Predicted.Test.svm$predict = ifelse(Predicted.Test.svm$predict < 0, 0, Predicted.Test.svm$predict)
out = c("datetime", "predict")
write.csv(Predicted.Test.svm[out], "data\\resultsSVM.csv", row.names = FALSE)







