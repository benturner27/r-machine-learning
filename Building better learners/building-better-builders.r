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
