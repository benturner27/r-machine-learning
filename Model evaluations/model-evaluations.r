#Better Model Evaluations

#Read csv to environment
sms_results <- read.csv("sms_results.csv")
head(sms_results)

#Retrieving results of which the prediction was unsure of the outcome
head(subset(sms_results, prob_spam > 0.40 & prob_spam < 0.60))

#Retrieving results of which the prediction was wrong
head(subset(sms_results, actual_type != predict_type))

#Utilising Confusion Matrices

#Building a simple confusion matrix manually
xtab = table(sms_results$actual_type, sms_results$predict_type)
xtab

#Building a confusion matrix using CrossTable
library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)

#Using CrossTable figures to calculate accuracy and error rate
(152 + 1203) / (152 + 1203 + 4 + 31) #accuracy percentage
(4 + 31) / (152 + 1203 + 4 + 31) #error rate
1 - 0.9748201 #error rate, same as 1 - accuracy

#Other measures of model performance

#Loading library caret
library(caret)

confusionMatrix(xtab, positive = "spam")

#Calculating kappa from CrossTable figures

#Calculating agreement between predictions and actual figures
pr_a <- 0.865 + 0.109
pr_a

#Calculating stimated agreement between predictions and actual figures
pr_e <- 0.868 * 0.888 + 0.132 * 0.112
pr_e

#Calculating Kappa
k <- (pr_a - pr_e) / (1 - pr_e)
k

#Retrieving kappa using vcd
library(vcd)
Kappa(table(sms_results$actual_type, sms_results$predict_type))

#Retrieving kappa using irr
library(irr)
kappa2(sms_results[1:2])

#Matthews correlation coefficient (MCC)

#Manually calculating the MCC from sms_results
(152 * 1203 - 4 * 31) /
  sqrt((152 + 4) * (152 + 31) * (1203 + 4) * (1203 + 31))

#Retrieving MCC using mltools
library(mltools)
mcc(sms_results$actual_type, sms_results$predict_type)

#Calculating MCC using cor function
cor(ifelse(sms_results$actual_type == "spam", 1, 0),
    ifelse(sms_results$predict_type == "spam", 1, 0))

#Sensitivity and specificity

#Manually calculating sensitivity (True Positive rate)
sens <- 152 / (152 + 31)
sens

#Manually calculating specificity (True Negative rate)
spec <- 1203 / (1203 + 4)
spec

#Retrieving sensitivity and specificity rate using caret
library(caret)
sensitivity(xtab, positive = "spam")
specificity(xtab, negative = "ham")

#Precision and Recall

#Calculating precision (Positive predictive value)
prec <- 152 / (152 + 4)
prec

#Caclulating recall
rec <- 152 / (152 + 31)
rec

#Retrieving precision and recall figures using caret
library(caret)
posPredValue(xtab, positive = "spam")
sensitivity(xtab, positive = "spam")

#F-measure

#Calculating the F-measure using the prec and rec figures
f <- (2 * prec * rec) / (prec + rec)
f

#Area Under ROC Curves (AUC)

#Load library pROC
library(pROC)

#Supplying the ROC with actual and prediction probabilities for visualisation
sms_roc <- roc(sms_results$actual_type, sms_results$prob_spam)

#Visualising ROC to a plot
plot(sms_roc, main = "ROC curve for SMS spam filter",
      col = "blue", lwd = 2, grid = TRUE, legacy.axes = TRUE)

#Comparing model to KNN model on same data
sms_results_knn = read.csv("sms_results_knn.csv")
sms_roc_knn = roc(sms_results$actual_type, sms_results_knn$p_spam)
plot(sms_roc_knn, col = "red", lwd = 2, add = TRUE)

#Retrieving AUC percentage on both models' ROC
auc(sms_roc)
auc(sms_roc_knn)

#Estimating future performance

#Read csv to environment
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)

#Create randomly generated list of 1000 ids for data frame
random_ids <- order(runif(1000))

#Splitting data frame into train, test and validation batches with random IDs
credit_train <- credit[random_ids[1:500], ]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

#Implementing stratified random sampling, ensuring that datasets have roughly
#the same proportion of data as the full dataset
library(caret)
in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_train <- credit[in_train, ]
credit_test <- credit[-in_train, ]

#Cross-validation

#Creating k-fold to maximise the amount of data available for the model
set.seed(123)
folds <- createFolds(credit$default, k = 10)
str(folds)

#Creating data for the first fold into the test and train sets
credit01_test <- credit[folds$Fold01, ]
credit01_train <- credit[-folds$Fold01, ]

#Automating the 10-fold CV
library(caret)
library(C50)
library(irr)

set.seed(123)
folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x){
  credit_train <- credit[-x, ]
  credit_test <- credit[x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

#Examining kappa results from 10-fold CV
str(cv_results)

#Calculating the mean kappa and standard deviation fro 10-fold CV
mean(unlist(cv_results))
sd(unlist(cv_results))
























