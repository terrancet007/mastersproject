#install.packages("randomForest")
library(randomForest)
#install.packages("caret")
library(caret)
#Load Dataset and explore
df.adm<-read.csv("adm.csv",header = TRUE)
str(df.nursey)
summary(df.nursey)


#Split the dataset into Train and Validation sets=70:30(random)
set.seed(100)
train<-sample(nrow(df.adm),0.7*nrow(df.adm))
df.train<-df.adm[train,]
df.valid<-df.adm[-train,]
summary(df.train)
summary(df.valid)  

#Create a Random forest model
modelRan<- randomForest(class ~ .,data=df.train,importance=TRUE)
modelRan

#Create a Random forest model with mtry=6
modelRan<- randomForest(class ~ .,data=df.train,mtry=6,importance=TRUE)

#Prediction on train set
trainPredict<-predict(modelRan,df.train,type="class")
trainPredict
table(trainPredict,df.train$class)
mean(trainPredict==df.train$class)

#Prediction on Test set
testPredict<-predict(modelRan,df.valid,type="class")
table(testPredict,df.valid$class)
mean(testPredict==df.valid$class)

#Calculate Cohen's Kappa,Sensitivity and Specificity
confusionMatrix(testPredict,df.valid$class)


#Decision Trees
#install.packages("rpart")
#install.packages("caret")
#install.packages("e1071")
library(rpart)
library(caret)
library(e1071)

dtmodel<-train(class ~.,data=df.train,method='rpart')
#Prediction on train set
dtmodel1<-predict(dtmodel,df.train)
table(dtmodel1,df.train$class)
mean(dtmodel1==df.train$class)


#Prediction on Validation set
dtmodel2<-predict(dtmodel,df.valid)
table(dtmodel2,df.valid$class)
mean(dtmodel2==df.valid$class)
