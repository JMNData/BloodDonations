#Add Libraries
library("e1071")
library("Metrics")
library(RODBC)
library("rpart")
library(randomForest)
library(lattice)
library(party)
library(ipred)

#Set Globals
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\BloodDonations")

#Read Files
Train = read.csv("train.csv", header = TRUE)
Test = read.csv("test.csv", header = TRUE)
Train = data.frame(Train[,-6], Made.Donation.in.March.2007=as.factor(Made.Donation.in.March.2007))
attach(Train)
#Analysis = c("X","Months.since.Last.Donation","Number.of.Donations","Total.Volume.Donated..c.c..","Months.since.First.Donation","Made.Donation.in.March.2007")
#Analysis = c("X","Months.since.Last.Donation","Number.of.Donations","Months.since.First.Donation","Made.Donation.in.March.2007")
#Train = Train[Analysis]

#Train 
model = bagging(Made.Donation.in.March.2007~., data=Train[,-1], method="dnn")
Predicted = cbind(Train, predicted = predict(model, Train[,-1]))
table(Predicted$Made.Donation.in.March.2007,Predicted$predicted)

#Test Data Output
Predicted = cbind(Test, predicted = predict(model, Test[,-1], type="prob"))
out = c("X", "predicted.1")
write.csv(Predicted[out], "results.csv", row.names = FALSE)




