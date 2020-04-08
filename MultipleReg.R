#install.packages("leaps")
library(caret)
#install.packages("DMwR")
library(DMwR)
#Read the data from csv file and load it in dataframe
df.avocadoPrice <-read.csv("avocado.csv",stringsAsFactors = F)
str(df.avocadoPrice)


#Split the observations into Training and Validation sets
#Training set : Validation set= 70:30
Train<- sample(nrow(df.avocadoPrice),0.70*nrow(df.avocadoPrice),replace = FALSE)
avocadoTrain<-df.avocadoPrice[Train,]
avocadoTest<-df.avocadoPrice[-Train,]

#Model Building
modelAvocado<-lm(AveragePrice~.,data=avocadoTrain)
predictAvocado<-predict(modelAvocado,avocadoTest)

#Verifying the model fit using R Sqaure
r.sqaure<-summary(modelAvocado)$r.squared
r.adj.sqaure<-summary(modelAvocado)$adj.r.squared


sprintf("In the Multiple regression model r.sqaure is :%0.2f",r.adj.sqaure)
errorRate<-(sigma(modelAvocado)/mean(df.avocadoPrice$AveragePrice)*100)
sprintf("In the Multiple regression model RSE is .2246 corresponding to percentage error rate :%.2f",errorRate)

#Prediction accuracy
actuals_preds <- data.frame(cbind(actual=avocadoTest$AveragePrice, predicted=predictAvocado))
correlation_accuracy<-cor(actuals_preds)
correlation_accuracy
head(actuals_preds)
#regr.eval(avocadoTest$AveragePrice,predictAvocado)

#Interaction effect
modelAvocadoInter<-lm(AveragePrice~Date+type+region+region*type,data=df.avocadoPrice)
r.sqaure<-summary(modelAvocadoInter)$r.squared
r.adj.sqaure<-summary(modelAvocadoInter)$adj.r.squared
sprintf("Interaction effect has improved the value of r.sqaure to :%0.3f from .68",r.adj.sqaure)
predictAvocadoInter<-predict(modelAvocadoInter,avocadoTest)


#Prediction accuracy on Interaction model
actuals_preds <- data.frame(cbind(actual=avocadoTest$AveragePrice, predicted=predictAvocadoInter))
correlation_accuracy<-cor(actuals_preds)
correlation_accuracy
head(actuals_preds)


#CrossValidation K-Fold
train_control<-trainControl(method='cv',number=10)
model<-train(AveragePrice~Date+type+region,data=df.avocadoPrice,trControl=train_control,method="lm")
print(model)
