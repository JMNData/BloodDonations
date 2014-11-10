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
mycontrol = rpart.control(cp = 0, xval = 20, surrogatestyle = 1)
model.rpart = rpart(Made.Donation.in.March.2007~., data=Train[,-1], method="class", control = mycontrol)
model.svm = svm(Made.Donation.in.March.2007~., data=Train[,-1], method="class", probability=FALSE, kernal="radial", gamma=1, cost=1)
model.rf = randomForest(Made.Donation.in.March.2007~., data=Train[,-1], method="class")
model.ct = ctree(Made.Donation.in.March.2007~., data=Train[,-1])

#RPART
Predicted.rpart = cbind(Train, predicted = predict(model.rpart, Train[,-1], type="class"))
Predicted.rpart = cbind(Train, predicted = predict(model.rpart, Train[,-1], type="prob"))
print("RPART")
table(Predicted.rpart$Made.Donation.in.March.2007,Predicted.rpart$predicted)
Predicted.rpart = cbind(Test, predicted = predict(model.rpart, Test[,-1], type="prob"))

out = c("X", "predicted.1")
write.csv(Predicted.rpart[out], "results.csv", row.names = FALSE)


#SVM
Predicted.svm = cbind(Train, predicted = predict(model.svm, Train[,-1]))
print("SVM")
table(Predicted.svm$Made.Donation.in.March.2007,Predicted.svm$predicted)

#RF
Predicted.rf = cbind(Train, predicted = predict(model.rf, Train[,-1], type="response"))
print("RandomForest")
table(Predicted.rf$Made.Donation.in.March.2007,Predicted.rf$predicted)

#CTree
Predicted.ct = cbind(Train, predicted = predict(model.ct, Train[,-1], type="prob"))
print("Inferencial Trees")
table(Predicted.ct$Made.Donation.in.March.2007,Predicted.ct$predicted)









