#Challenging Data

#Using stepwise regression for feature selection

#Preparing dataset for stepwise regression process
library(tidyverse)
titanic_train <- read_csv("titanic_train.csv") |>
  mutate(
    Age_MVI = if_else(is.na(Age), 1, 0),
    Age = if_else(is.na(Age), mean(Age, na.rm = TRUE), Age),
    Cabin = if_else(is.na(Cabin), "X", Cabin),
    Embarked = factor(if_else(is.na(Embarked), "X", Embarked)),
    Sex = factor(Sex)
  )

#Defining the minimal model needed for the stepwise regression process
simple_model <- glm(Survived ~ 1, family = "binomial",
                    data = titanic_train)

#Defining the full model for the stepwise regression process
full_model <- glm(Survived ~ Age + Age_MVI + Embarked +
                    Sex + Pclass + SibSp + Fare,
                  family = "binomial", data = titanic_train)

#Implementing the stepwise regression model for determining best variable
sw_forward <- stats::step(simple_model,
                          scope = formula(full_model),
                          direction = "forward")

#Acquiring formula for final model and beta coefficients
formula(sw_forward)
sw_forward$coefficients

#Implementing a backwards regression model for removing the worst variables
sw_backward <- stats::step(full_model, direction = "backward")

#Using Boruta for feature selection

#Adding a random values feature to the dataset
set.seed(12345)
titanic_train$rand_vals <- runif(n = 891, min = 1, max = 100)

#Implementing Boruta to find the most important variables
library(Boruta)
titanic_boruta <- Boruta(Survived ~ PassengerId + Age +
                           Sex + Pclass + SibSp + rand_vals,
                         data = titanic_train, doTrace = 1)

#Exploring the results of the Boruta model
titanic_boruta

#Plotting the results of feature selection from Boruta model
plot(titanic_boruta)


























