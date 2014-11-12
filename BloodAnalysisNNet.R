#Add Libraries
library("e1071")
library("Metrics")
library(RODBC)
library("rpart")
library(randomForest)
library(lattice)
library(party)
library(ipred)
library(caret)

#Set Globals
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\BloodDonations")

#Read Files
Train = read.csv("train.csv", header = TRUE)
Test = read.csv("test.csv", header = TRUE)
#Train = data.frame(Train[,-6], Made.Donation.in.March.2007=as.factor(Made.Donation.in.March.2007))
attach(Train)
#Analysis = c("X","Months.since.Last.Donation","Number.of.Donations","Total.Volume.Donated..c.c..","Months.since.First.Donation","Made.Donation.in.March.2007")
#Analysis = c("X","Months.since.Last.Donation","Number.of.Donations","Months.since.First.Donation","Made.Donation.in.March.2007")
#Train = Train[Analysis]


#Train 
#model = svm(Made.Donation.in.March.2007~., data=Train[,-1], kernel="radial")
model <- nnet(Made.Donation.in.March.2007 ~ .,data=Train,size=10, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

#Test Data Output
Predicted = cbind(Test, predicted = predict(model, Test[,-1], type="raw"))

out = c("X", "predicted")
output = ',Made Donation in March 2007'
write.table(output, file="results.csv", append= FALSE, row.names = FALSE, col.names=FALSE, sep=",", eol="\r", quote=FALSE)
write.table(Predicted[out], file="results.csv", append=TRUE, row.names = FALSE, col.names=FALSE, sep=",", eol="\r")



