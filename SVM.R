install.packages("e1071")
install.packages("caTools")
install.packages("pscl")
install.packages("ROCR")
install.packages("caret")
library(e1071)
library(dplyr)
library(caret)
library(caTools)
library(ROCR)
library(randomForest)


cardiosvm <- read.csv("C:/Users/Tero/Desktop/cardio_train.csv", header = TRUE, sep = ";")
str(cardiosvm)
dim(cardiosvm)

cardiosvm$cardio <- as.factor(cardiosvm$cardio) 
cardiosvm$active <- as.factor(cardiosvm$active) 
cardiosvm$alco <- as.factor(cardiosvm$alco) 
cardiosvm$smoke <- as.factor(cardiosvm$smoke) 



set.seed(888)
split = sample.split(cardiosvm, SplitRatio = 0.75)
cardiosvmTrain<-subset(cardiosvm,split==TRUE)
cardiosvmTest<-subset(cardiosvm,split==FALSE)

#svm

mymodel <- svm(cardio ~., data = cardiosvmTrain)
summary(mymodel)



#confusionmatrix
pred <- predict(mymodel,cardiosvmTrain)
tab <- table(predicted = pred, actual = cardiosvmTrain$cardio)
tab
confusionMatrix(pred,cardiosvmTrain$cardio)
accuracy <- sum(diag(tab))/sum(tab)
accuracy

#tryin with test data now

svm.model <- svm(cardio ~ ., data = cardiosvmTrain, trControl=train_control)
p <- predict(svm.model, newdata=cardiosvmTest, type = "response")
#svmPerformance(svm.model, test_svm, test_svm$cardio)
#Accuracy [1] 0.7272251
#class(test_svm$cardio)
#class(svm.model)
library(e1071)

#confusionmatrix
confusionMatrix(p, cardiosvmTest$cardio)


p <- predict(svm.model, newdata=cardiosvmTest, type = "response")
tabnew <- table(predictednew = p, actual = cardiosvmTest$cardio)
tabnew

accuracynew <- sum(diag(tabnew))/sum(tabnew)
accuracynew

