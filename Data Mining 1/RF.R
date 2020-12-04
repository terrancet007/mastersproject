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


cardio <- read.csv("C:/Users/Tero/Desktop/cardio_train.csv", header = TRUE, sep = ";")
str(cardio)
dim(cardio)

cardio$cardio <- as.factor(cardio$cardio) 
cardio$active <- as.factor(cardio$active) 
cardio$alco <- as.factor(cardio$alco) 
cardio$smoke <- as.factor(cardio$smoke) 



set.seed(777)
split = sample.split(cardio, SplitRatio = 0.75)
cardioTrain<-subset(cardio,split==TRUE)
cardioTest<-subset(cardio,split==FALSE)

dim(cardioTrain)
dim(cardioTest)

sapply(cardioTrain,function(x) sum(is.na(x)))



#RF <- randomForest(cardio ~., data=cardioTrain, ntree= 276, mtry= 3)
RF <- randomForest(cardio ~., data=cardioTrain)
print(RF)

RF$confusion

prediction1<- predict(RF,cardioTrain)
head(prediction1)
head(cardioTrain$cardio)
confusionMatrix(prediction1,cardioTrain$cardio)

prediction2<- predict(RF,cardioTest)
confusionMatrix(prediction2,cardioTest$cardio)

#error rate
plot(RF)

#tune mtry

tuneRF(cardioTrain[,-13],cardioTrain[,13], stepFactor = 0.5,plot = TRUE,
       ntreeTry = 276,trace = TRUE,improve = 0.05)


