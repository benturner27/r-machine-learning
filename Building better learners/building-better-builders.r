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
