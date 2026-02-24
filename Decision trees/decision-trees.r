#Decision Trees

#Understanding decision trees - Entropy example

#Calculating the entropy between two classes
#Where the dataset is split between red (60%) and white (40%)
-0.60 * log2(0.60) - 0.40 * log2(0.40)

#Visualising entropy levels for all two-class arrangements using curve function
curve(-x * log2(x) - (1 - x) * log2(1 - x),
      col = "red", xlab = "x", ylab = "entropy", lwd = 4)

#Data Preparation

#Data analysis

#Read csv to file, enabling StringAsFactors parameter to true, since all data is
#categorical
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
str(credit)

#Exploring trends for loans by analysing bank accounts and savings using table
#function
table(credit$checking_balance)
table(credit$savings_balance)

#Analysing loan duration and loan amount using summary function
summary(credit$months_loan_duration)
summary(credit$amount)

#Checking the proportion of applicants have defaulted using table
table(credit$default)

#Splitting and creating random training and testing sets

#Creating training and testing datasets using set.seed and sample functions.
#Needed for this case so model is able to test for any application no matter
#the amount, duration or default status for accurate modelling
set.seed(9829)
train_sample <- sample(1000, 900)
str(train_sample)

#Creating training and testing datasets
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]

#Testing for representative data for defaults using prop.table
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

#Data modelling

#Loading library C50
library(C50)

#Training the C5.0 Decision Trees model on credit_train dataset
credit_model <- C5.0(default ~ ., data = credit_train)

#Analysing the tree's decisions
summary(credit_model)

#Evaluating model performance

#Applying training model onto testing data using predict function C.50
credit_pred <- predict(credit_model, credit_test)

#Using CrossTable to evaluate results with data in data frame
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c("Actual default", "Predicted result"))

#Improving model performance

#Boosting DT accuracy

#Specifying 10 trials to be used for data modelling to limit the amount of
#unnecessary splits in data
credit_boost <- C5.0(default ~ ., data = credit_train,
                     trials = 10)

#Applying the boosted model to the testing data and evaluating its performance
credit_boost_pred2 <- predict(credit_boost, credit_test)
CrossTable(credit_test$default, credit_boost_pred2,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c("Actual result", "Predicted result"))
