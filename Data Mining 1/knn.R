install.packages("dplyr")
install.packages("tidyr")
install.packages("tibble")
install.packages("knitr")
install.packages("plotROC")
install.packages("corrplot")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("caret")
install.packages("ggplot2")
install.packages("fastDummies")
install.packages("plotly")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("kernlab")
install.packages("gridExtra")

library(dplyr)
library(tidyr)
library(tibble)
library(knitr)
library(plotROC)
library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(ggplot2)
library(fastDummies)
library(plotly)
library(FactoMineR)
library(factoextra)
library(kernlab)
library(gridExtra)


accidents <- read.csv("C://Users//Tero//Desktop//Accidents_categorical.csv", header=TRUE)

#removing unwanted columns
accidents <- as_tibble(accidents[ , !(names(accidents) %in% c('Accident_Index', 'Latitude',
                                                                       'Longitude', 'Datetime'))])

#sampling the data
set.seed(17143845)
sampled_df_categorical <- sample_n(accidents, 20000)       #sampled_df_categorical

sample <- createDataPartition(sampled_df_categorical$Accident_Severity, p=0.5, list=FALSE)  #sample

sampled_df_numerical <- sampled_df_categorical[sample, ]  #sampled_df_numerical


#removing the noise
sampled_df_numerical <- as_tibble(sampled_df_numerical[ , !(names(sampled_df_numerical) %in%
                                                              c('High_Wind', 'Propulsion_Code'))])

sampled_df_numerical <- dummy_cols(sampled_df_numerical,
                                   select_columns = c('Region', 'X1st_Road_Class', 'Urban_or_Rural_Area',
                                                      'Road_Surface_Conditions', 'Road_Type', 'Weather',
                                                      'Lights', 'Junction_Detail',
                                                      'Junction_Location', 'X1st_Point_of_Impact',
                                                      'Driver_Journey_Purpose', 'Vehicle_Make',
                                                      'Vehicle_Category', 'Vehicle_Manoeuvre'),
                                   remove_most_frequent_dummy=TRUE)


columns_to_drop <- c('Region', 'X1st_Road_Class', 'Urban_or_Rural_Area', 'Road_Surface_Conditions', 'Road_Type',
                     'Weather', 'Lights', 'Junction_Detail', 'Junction_Location', 'X1st_Point_of_Impact',
                     'Driver_Journey_Purpose', 'Vehicle_Make', 'Vehicle_Category', 'Vehicle_Manoeuvre')

sampled_df_numerical <- sampled_df_numerical[ , !(names(sampled_df_numerical) %in% columns_to_drop)]
rm(columns_to_drop)
sampled_df_normalized <- predict(preProcess(sampled_df_numerical, method=c("center", "scale")),
                                 sampled_df_numerical)

sampled_df_normalized <- dummy_cols(sampled_df_normalized, select_columns = c('Accident_Severity'),
                                    remove_most_frequent_dummy=TRUE)

names(sampled_df_normalized)[names(sampled_df_normalized) == 'Accident_Severity_Fatal_Serious'] <- 'Fatal_or_Serious_Accident'
sampled_df_numerical <- select(sampled_df_normalized, -matches("Accident_Severity"))
sampled_df_normalized <- select(sampled_df_normalized, -matches("Fatal_or_Serious_Accident"))

#knn being applied

set.seed(17143845)
sample <- createDataPartition(sampled_df_normalized$Accident_Severity, p=0.7, list=FALSE) 
train_df_normalized <- sampled_df_normalized[sample, ]
test_df_normalized <- sampled_df_normalized[-sample, ]
accuracy_baseline <- round(length(test_df_normalized[test_df_normalized$Accident_Severity == 'Slight', ]$Accident_Severity)/
                             length(test_df_normalized$Accident_Severity), 3)
accuracy_baseline
  
  
  
#Training the model
start_time <- Sys.time()
knnFit <- train(Accident_Severity~., data=train_df_normalized, method="knn", tuneGrid=expand.grid(k=25:50),
                metric="Accuracy", trControl=trainControl(method="cv", number=10))
end_time <- Sys.time()
knn_runtime <- round(as.numeric(end_time - start_time), 2)
plot(knnFit)
  


#accuracy test
test_df_normalized$pred <- predict(knnFit, test_df_normalized)
knn_confusion_matrix <- confusionMatrix(data=test_df_normalized$pred, reference=test_df_normalized$Accident_Severity)
test_df_normalized <- test_df_normalized %>%
  mutate(pred = predict(knnFit, test_df_normalized),
         pred_prob = predict(knnFit, type="prob", test_df_normalized)[,2],
         error = ifelse(pred != Accident_Severity, 1, 0))
roc <- test_df_normalized %>%
  select(Accident_Severity, pred_prob) %>%
  mutate(Accident_Severity = as.numeric(Accident_Severity) - 1,
         Accident_Severity.str = c("Fatal_Serious", "Slight")[Accident_Severity + 1]) %>%
  ggplot(aes(d = Accident_Severity, m = pred_prob)) +
  geom_roc(labels = FALSE)
roc +
  style_roc(theme = theme_bw, xlab = "False Positive Rate", ylab = "True Positive Rate") +
  theme(panel.grid.major = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "grey")) +
  ggtitle("kNN - ROC Curve") +
  annotate("text", x = .75, y = .25,
           label = paste("AUROC =", round(calc_auc(roc)$AUC, 3)))
knn_accuracy <- round(knn_confusion_matrix$overall['Accuracy'], 3)
knn_precision <- round(knn_confusion_matrix$byClass['Pos Pred Value'], 3)
knn_recall <- round(knn_confusion_matrix$byClass['Sensitivity'], 3)
knn_f1_score <- round(2*((knn_precision * knn_recall) / (knn_precision + knn_recall)), 3)
knn_roc <- round(calc_auc(roc)$AUC, 3)
test_df_normalized <- test_df_normalized[ , !(names(test_df_normalized) %in% c('pred', 'pred_prob', 'error'))]
data.frame(accuracy_baseline, knn_accuracy, knn_precision, knn_recall, knn_f1_score, knn_roc)
