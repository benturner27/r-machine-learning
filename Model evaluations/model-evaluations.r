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

