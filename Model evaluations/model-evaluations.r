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
table(sms_results$actual_type, sms_results$predict_type)

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
confusionMatrix(sms_results$predict_type,
                sms_results$actual_type, positive = "spam")

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
sensitivity(sms_results$predict_type, sms_results$actual_type,
            positive = "spam")
specificity(sms_results$predict_type, sms_results$predict_type,
            negative = "ham")

#Precision and Recall

#Calculating precision (Positive predictive value)
prec <- 152 / (152 + 4)
prec

#Caclulating recall
rec <- 152 / (152 + 31)
rec

#Retrieving precision and recall figures using caret
library(caret)
posPredValue(sms_results$predict_type, sms_results$actual_type,
             positive = "spam")
sensitivity(sms_results$predict_type, sms_results$actual_type,
            positive = "spam")

#F-measure

#Calculating the F-measure using the prec and rec figures
f <- (2 * prec * rec) / (prec + rec)
f
