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
#Train = data.frame(Train[,-6], Made.Donation.in.March.2007=as.factor(Made.Donation.in.March.2007))
attach(Train)
#Analysis = c("X","Months.since.Last.Donation","Number.of.Donations","Total.Volume.Donated..c.c..","Months.since.First.Donation","Made.Donation.in.March.2007")
#Analysis = c("X","Months.since.Last.Donation","Number.of.Donations","Months.since.First.Donation","Made.Donation.in.March.2007")
#Train = Train[Analysis]


#Train 
model = glm(Made.Donation.in.March.2007~., data=Train[,-1], family=inverse.gaussian)
deviance(model)
Predicted = cbind(Train, predicted = predict(model, Train[,-1]))
table(Predicted$Made.Donation.in.March.2007,Predicted$predicted)
model$tables


?glm

#Test Data Output
Predicted = cbind(Test, predicted = round(predict(model, Test[,-1]), digits=6))

out = c("X", "predicted")
output = ',Made Donation in March 2007'
write.table(output, file="results.csv", append= FALSE, row.names = FALSE, col.names=FALSE, sep=",", eol="\r", quote=FALSE)
write.table(Predicted[out], file="results.csv", append=TRUE, row.names = FALSE, col.names=FALSE, sep=",", eol="\r")





