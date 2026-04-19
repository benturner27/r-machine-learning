#Advanced Data Preparation

#Table structures

#Creating a tibble using titanic.csv
library(tibble)
titanic_csv <- read.csv("titanic_train.csv")
titanic_tbl <- as_tibble(titanic_csv)

#Reading rectangular files

#Creating a tibble directly from csv file format
library(readr)
titanic_csv <- read_csv("titanic_train.csv")

#Creating a tibble directly from xlsx file format
library(readxl)
titanic_train <- read_excel("titanic_train.xlsx")

#Preparing and piping data

#Piping dataset to only view women
library(dplyr)
titanic_train |> filter(Sex == "female")

#Piping dataset to select name, sex and age columns
titanic_train |> select(Name, Sex, Age)

titanic_women <- titanic_train |> 
  filter(Sex == "female") |>
  select(Name, Sex, Age) |>
  arrange(Name)

#Creating a elderly feature from dataset
titanic_train |> 
  mutate(elderly = if_else(Age >= 65, 1, 0))

#Creating a child feature from dataset
titanic_train |>
  mutate(
    elderly = if_else(Age >= 65, 1, 0),
    child = if_else(Age < 18, 1 , 0)
  )

#Calculating survival rates by sex
titanic_train |>
  group_by(Sex) |>
  summarize(survival_rate = mean(Survived))

#Calculating survival rates of children
titanic_train |>
  filter(!is.na(Age)) |>
  mutate(child = if_else(Age < 18, 1, 0)) |>
  group_by(child) |>
  summarize(survival_rate = mean(Survived))

#Building decision trees model on titanic dataset
library(rpart)
m_titanic <- titanic_train |>
  filter(!is.na(Age)) |>
  mutate(AgeGroup = if_else(Age < 18, "Child", "Adult")) |>
  select(Survived, Pclass, Sex, AgeGroup) |>
  rpart(formula = Survived ~ ., data = _)

#Visualising the decision trees model
library(rpart.plot)
rpart.plot(m_titanic)































