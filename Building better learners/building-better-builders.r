#Building better learners

#Building a simple tuned model
library(tidyverse)
library(caret)
credit <- read_csv("credit.csv")
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")
m

#Using the tuned model to predict on the dataset
p <- predict(m, credit)
table(p, credit$default)

#Examining the predicion results and it's probabilities on the outcome
head(predict(m, credit))
head(predict(m, credit, type = "prob"))

#Tuning customisation

#Using trainControl function to customise tuning process
ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")

#Creating a search grid for hyperparameter tuning using expand.grid function
grid <- expand.grid(model = "tree",
                    trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    winnow = FALSE)

#Experimenting with models defined in grid to determine best one
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
m

#Ensemble learning

#Using the bootstrap aggregating (bagging) to create an ensemble of decision
#tree models
library(ipred)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(123)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

#Running the bagging method on a 10-fold CV fore better real-world performance
library(caret)
credit <- read.csv("credit.csv")
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag", trControl = ctrl)

#Boosting 

#Example of using a boosting ensemble with AdaBoost
library(adabag)
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m_adaboost <- boosting(default ~ ., data = credit)
p_adaboost <- predict(m_adaboost, credit)

#Viewing and examining the results of the predictions made by the model
head(p_adaboost$class)
p_adaboost$confusion

#Using a 10-fold CV to more accurately evaluate this model's performance
set.seed(300)
adaboost_cv <- boosting.cv(default ~ ., data = credit)
adaboost_cv$confusion

#Retrieving the kappa values from the training model
library(vcd)
Kappa(adaboost_cv$confusion)



























