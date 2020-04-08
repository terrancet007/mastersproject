install.packages("caTools")
install.packages("ROCR")
install.packages("caret")

library(caret)
library(caTools)
library(ROCR)
#Read the data from csv file and load it in dataframe
df_bank <-read.csv("C://Users//Tero//Desktop//bank-additional-full.csv",sep = ";",stringsAsFactors = TRUE)
str(df_bank)
dim(df_bank)


#Splitting the data into Train sets and Testing sets
#Traning set:Testing set =70:30(random)
set.seed(100)
split = sample.split(df_bank, SplitRatio = 0.70)
bankTrain<-subset(df_bank,split==TRUE)
bankTest<-subset(df_bank,split==FALSE)

table(bankTrain$y)
table(bankTest$y)

#Model fitting and parameter estimation on traning set
mod_fit<-glm(y~default+contact+month+day_of_week+duration+
               campaign+pdays+poutcome+emp.var.rate+cons.price.idx+
               euribor3m,family=binomial(link='logit'),data=bankTrain)

varImp(mod_fit)
summary(mod_fit)

#Prediction on test data set
predSubscribe = predict(mod_fit, bankTest,type="response")

bankTest$y<-ifelse(bankTest$y=="yes",1,0)
bankTest$y<-factor(bankTest$y,levels=c(0,1))
y_pred_no<-ifelse(predSubscribe>0.5,1,0)
y_pred<-factor(y_pred_no,levels = c(0,1))
y_pred
confusionMatrix(y_pred,bankTest$y)


#Calculate Sensivity and Specificity
accuracy <- table(y_pred, bankTest$y)
sprintf("Prediction accuracy i.e. True Positive+True Negative= %0.2f",sum(diag(accuracy))/sum(accuracy))
confTable<-confusionMatrix(y_pred, bankTest$y)
str(bankTest$y)

#ROC curve
predROCR<-prediction(predSubscribe,bankTest$y)
ROCRperf <- performance(predROCR, 'tpr','fpr')
plot(ROCRperf,colorize=TRUE)

  
#Calculate F-score
precisionPerctange  <- (confTable$table[1,1]/((confTable$table[1,1])+(confTable$table[2,1])))
precisionPerctange
recallPercentage  <- (confTable$table[1,1]/((confTable$table[1,1])+(confTable$table[1,2])))
recallPercentage
Fscore <- (2*precisionPerctange*recallPercentage)/(precisionPerctange+recallPercentage)
sprintf("F Score -considering the precision and recall values together is %0.2f",Fscore)



#K-Fold cross Validation
  train_control<-trainControl(method='repeatedcv',number=10,
                              savePredictions = TRUE)

model<-train(y ~ default+contact+month+day_of_week+duration+
               campaign+pdays+poutcome+emp.var.rate+cons.price.idx+
               euribor3m,data=bankTrain,method="glm",family="binomial",trControl=train_control,tuneLength=5)

predCV=predict(model,bankTest)

#table(predCV>0.5,bankTest$y)

y_pred_cv<-ifelse(predCV=='yes',1,0)
y_pred_cv<-factor(y_pred_cv,levels=c(0,1))
y_pred_cv
confusionMatrix(y_pred_cv,bankTest$y)
