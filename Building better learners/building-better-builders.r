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

#Random Forest

#Creating a Random Forest model to train on the credit dataset
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data= credit)
rf

#Retrieving the kappa value from the trained model
library(vcd)
Kappa(rf$confusion[1:2,1:2])

#Creating a Random Forest model using the ranger package
library(ranger)
set.seed(300)
m_ranger <- ranger(default ~ ., data = credit)
m_ranger

#Retrieving the kappa value from the trained model
Kappa(m_ranger$confusion.matrix)

#Gradient boosting

#Creating a gradient boosted model for the credit dataset
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
credit$default <- ifelse(credit$default == "yes", 1, 0)
set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
library(gbm)
set.seed(300)
m_gbm <- gbm(default ~ ., data = credit_train)
m_gbm

#Running the trained model on the test split, as well as converting the
#probabilities into classifications
p_gbm <- predict(m_gbm, credit_test, type = "response")
p_gbm_c <- ifelse(p_gbm > 0.50, 1, 0)
table(credit_test$default, p_gbm_c)

#Applying kappa values to the prediction results
library(vcd)
Kappa(table(credit_test$default, p_gbm_c))

#Creating a search grid to define the candidate GBM models to find the best one
grid_gbm <- expand.grid(n.trees = c(100, 150, 200),
                        interaction.depth = c(1, 2, 3),
                        shrinkage = c(0.1, 0.2, 0.3),
                        n.minobsinnode = 10)

#Setting trainControl to select best model from a 10-fold CV
library(caret)
ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "best")

#Determining the best model from the search grid using the control parameters
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m_gbm_c <- train(default ~ ., data = credit, method = "gbm",
                 trControl = ctrl, tuneGrid = grid_gbm, metric = "Kappa",
                 verbose = FALSE)
m_gbm_c












