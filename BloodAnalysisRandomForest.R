#Add Libraries
library("e1071")
library("Metrics")
library(RODBC)
library("rpart")
library(randomForest)
library(lattice)
library(party)

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

#Train Data
model.rf = randomForest(Made.Donation.in.March.2007~., data=Train[,-1], method="class")
importance(model.rf)



model.cf = cforest(Made.Donation.in.March.2007~., data=Train[,-1], controls=cforest_unbiased(ntree=2000, mtry=3))


#RF
Predicted.rf = cbind(Train, predicted = predict(model.rf, Train[,-1], type="response"))
print("RandomForest")
table(Predicted.rf$Made.Donation.in.March.2007,Predicted.rf$predicted)

Predicted.cf = cbind(Train, predicted = predict(model.cf, Train[,-1], OOB=TRUE, type="response"))
print("CForest")
table(Predicted.cf$Made.Donation.in.March.2007,Predicted.cf$predicted)

#Test Data Output
Predicted.cf = cbind(Test, predicted = predict(model.cf, Test[,-1], OOB=TRUE, type="prob"))
out = c("X", "predicted.1")
write.csv(Predicted.rf[out], "results.csv", row.names = FALSE)



?predict



