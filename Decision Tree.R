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


#data loading
accidents_df <- read.csv("C://Users//Tero//Desktop//Accidents_categorical.csv", header=TRUE)
accidents_df <- as_tibble(accidents_df[ , !(names(accidents_df) %in% c('Accident_Index', 'Latitude',
                                                                       'Longitude', 'Datetime'))])

#data sampling
set.seed(17143845)
sampled_df_categorical <- sample_n(accidents_df, 20000)



#decision tree


set.seed(17143845)
sample <- createDataPartition(sampled_df_categorical$Accident_Severity, p=0.7, list=FALSE) 
train_df_categorical <- sampled_df_categorical[sample, ]
test_df_categorical <- sampled_df_categorical[-sample, ]
accuracy_baseline <- round(length(test_df_categorical[test_df_categorical$Accident_Severity == 'Slight', ]$Accident_Severity)/
                             length(test_df_categorical$Accident_Severity), 3)
accuracy_baseline




# Unpruned Decision tree
options(warn=-1)
start_time <- Sys.time()
unpruned_decision_tree <- rpart(Accident_Severity~., data=train_df_categorical, method="class", 
                                minbucket=1, minsplit=1, control=rpart.control(cp=0.0001))
png("Unpruned DecisionTree.png", width=1920, height=1080, res=400)
rpart.plot(unpruned_decision_tree, main="Unpruned - Road accident severity in UK",
           extra=104, branch.lty=3, split.cex=1.2, type=3)
dev.off()
test_df_categorical$pred <- predict(unpruned_decision_tree, test_df_categorical, type="class")
end_time <- Sys.time()
dt_runtime <- round(as.numeric(end_time - start_time)/60, 2)
unpruned_accuracy <- round(mean(test_df_categorical$pred == test_df_categorical$Accident_Severity), 3)


# Post-pruned Decision tree
cp <- as_tibble(unpruned_decision_tree$cptable) %>%
  filter(xerror <= min(xerror) + xstd) %>%
  filter(xerror == max(xerror)) %>%
  select(CP) %>%
  unlist()
pruned_decision_tree <- prune(unpruned_decision_tree, cp=cp)
rpart.plot(pruned_decision_tree, main="Pruned - Road accident severity in UK",
           extra=104, branch.lty=3, split.cex=1.2, type=3)


# Accuracy of unpruned and pruned tree
test_df_categorical$pred <- predict(pruned_decision_tree, test_df_categorical, type="class")
pruned_accuracy <- round(mean(test_df_categorical$pred == test_df_categorical$Accident_Severity), 3)
data.frame(accuracy_baseline, unpruned_accuracy, pruned_accuracy)




dt_confusion_matrix <- confusionMatrix(data=test_df_categorical$pred, reference=test_df_categorical$Accident_Severity)
test_df_categorical <- test_df_categorical %>%
  mutate(pred = predict(pruned_decision_tree, type="class", test_df_categorical),
         pred_prob = predict(pruned_decision_tree, type="prob", test_df_categorical)[,2],
         error = ifelse(pred != Accident_Severity, 1, 0))
roc <- test_df_categorical %>%
  select(Accident_Severity, pred_prob) %>%
  mutate(Accident_Severity = as.numeric(Accident_Severity) - 1,
         Accident_Severity.str = c("Fatal_Serious", "Slight")[Accident_Severity + 1]) %>%
  ggplot(aes(d = Accident_Severity, m = pred_prob)) +
  geom_roc(labels = FALSE)
roc +
  style_roc(theme = theme_bw, xlab = "False Positive Rate", ylab = "True Positive Rate") +
  theme(panel.grid.major = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "grey")) +
  ggtitle("Decision Tree - ROC Curve") +
  annotate("text", x = .75, y = .25,
           label = paste("AUROC =", round(calc_auc(roc)$AUC, 3)))
dt_accuracy <- round(dt_confusion_matrix$overall['Accuracy'], 3)
dt_precision <- round(dt_confusion_matrix$byClass['Pos Pred Value'], 3)
dt_recall <- round(dt_confusion_matrix$byClass['Sensitivity'], 3)
dt_f1_score <- round(2*((dt_precision * dt_recall) / (dt_precision + dt_recall)), 3)
dt_roc <- round(calc_auc(roc)$AUC, 3)
test_df_categorical <- test_df_categorical[ , !(names(test_df_categorical) %in% c('pred', 'pred_prob', 'error'))]
data.frame(accuracy_baseline, dt_accuracy, dt_precision, dt_recall, dt_f1_score, dt_roc)


dt_importance <- data.frame(Variables=names(pruned_decision_tree$variable.importance),
                            Variables_Importance=pruned_decision_tree$variable.importance)
dt_importance_plot <- ggplot(dt_importance, aes(x=reorder(Variables, Variables_Importance), y=Variables_Importance, fill=Variables_Importance)) +
  geom_bar(stat='identity') + coord_flip() + theme(legend.position="none") + labs(x="") +
  ggtitle('Variable Importance in the Decision Tree model') + theme(plot.title = element_text(hjust=0.5))
dt_importance_plot
rm(cp, dt_importance)