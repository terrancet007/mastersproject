library(caret)
library(caTools)
library(pscl)
library(class)
library(ROCR)


#Read the data from csv file and load it in dataframe
df_bank <-read.csv("bank-additional-full.csv",sep = ";",stringsAsFactors = FALSE)

df_bank$y<-ifelse(df_bank$y=="yes",1,0)


#Normalization of numeric features
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

df_bank[11:14]<-as.data.frame(lapply(df_bank[,11:14], normalize))
df_bank[16:20]<-as.data.frame(lapply(df_bank[,16:20], normalize))


#Extracting only the relevant columns
df_bank.subset<-cbind(df_bank[,16:21],df_bank[,11:14])

str(df_bank.subset)
#Traning set:Testing set =70:30(random)
set.seed(88)

train<-sample(nrow(df_bank.subset),0.75*nrow(df_bank.subset),replace = FALSE)
bankTrain<-df_bank.subset[train,]
bankTest<-df_bank.subset[-train,]
str(bankTest)
bankTrainLabels<-df_bank.subset[train,6]
bankTestLabels<-df_bank.subset[-train,6]
table(bankTrainLabels)
table(bankTestLabels)

#KNN Model fitting on train set with value of k=202
knn.202<-knn(bankTrain,bankTest,cl=bankTrainLabels,k=202)

bankTestLabels<-factor(bankTestLabels,levels=c(0,1))


#Confusion matrix
confusionMatrix(knn.202,bankTestLabels)
confTable<-confusionMatrix(knn.202,bankTestLabels)



#F- Score Calculation
precisionPerctange  <- (confTable$table[1,1]/((confTable$table[1,1])+(confTable$table[2,1])))
precisionPerctange
recallPercentage  <- (confTable$table[1,1]/((confTable$table[1,1])+(confTable$table[1,2])))
recallPercentage
Fscore <- (2*precisionPerctange*recallPercentage)/(precisionPerctange+recallPercentage)
sprintf("F Score -considering the precision and recall values together is %0.2f",Fscore)
