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

#Text transformation

#Determining cabin codes of passengers by class
library(stringr)
titanic_train <- titanic_train |>
  mutate(CabinCode = str_sub(Cabin, start = 1, end = 1))

#Exploring whether CabinCode relationship is meaningful
table(titanic_train$Pclass, titanic_train$CabinCode, useNA = "ifany")

#Determining whether cabin codes relates to higher survival rates by class
library(ggplot2)
titanic_train |> ggplot() +
  geom_bar(aes(x = CabinCode, y = Survived),
           stat = "summary", fun = "mean") +
  ggtitle("Titanic Survival Rate by Cabin Code")

#Creating title feature by matching prefixes to reg ex
titanic_train <- titanic_train |>
  mutate(Title = str_extract(Name, ", [A-z]+\\."))

#Replacing and removing the punctuation in the prefixes using reg ex
titanic_train <- titanic_train |>
  mutate(Title = str_replace_all(Title, "[, \\.]", ""))
table(titanic_train$Title)

#Creating TitleGroup feature to simplify the amount of prefixes available
titanic_train <- titanic_train |>
  mutate(TitleGroup  = recode(Title,
    "Mr" = "Mr", "Mrs" = "Mrs", "Master" = "Master",
    "Miss" = "Miss", "Ms" = "Miss", "Mlle" = "Miss",
    "Mme" = "Miss", .missing = "Other", .default = "Other"))

#Verifying that TitleGroup has been implemented properly
table(titanic_train$TitleGroup)

#Determining whether relationship between title and survival rates is meaningful
titanic_train |> ggplot() +
  geom_bar(aes(x = TitleGroup, y = Survived),
           stat = "summary", fun = "mean") +
  ggtitle("Survival Rates by Title Group")

#Date cleaning

#Example of lubridate's date constructor
library(lubridate)
mdy(c("October 25, 2013", "10/25/2013"))
dmy(c("25 October 2013", "25.10.2013"))
ymd("2013-10-25")

#Constructing date objects
MLwR_1stEd <- mdy("October 25, 2013")
MLwR_2ndEd <- mdy("July 31, 2015")
MLwR_3rdEd <- mdy("April 15, 2019")

#Computing differences between 2 dates
MLwR_2ndEd - MLwR_1stEd
MLwR_3rdEd - MLwR_2ndEd

#Converting the day calculation into seconds
as.duration(MLwR_2ndEd - MLwR_1stEd)
as.duration(MLwR_3rdEd - MLwR_2ndEd)

#Converting the as.duration calculation into just years
dyears()
as.duration(MLwR_2ndEd - MLwR_1stEd) / dyears()
as.duration(MLwR_3rdEd - MLwR_2ndEd) / dyears()

#Using time_length function to achieve the same calculation
time_length(MLwR_2ndEd - MLwR_1stEd, unit = "years")
time_length(MLwR_3rdEd - MLwR_2ndEd, unit = "years")

#Example of interval function
USA_DOB <- mdy("July 4, 1776")
time_length(mdy("July 3, 2023") - USA_DOB, unit = "years")
time_length(mdy("July 5, 2023") - USA_DOB, unit = "years")

interval(USA_DOB, mdy("July 3, 2023")) / years()
interval(USA_DOB, mdy("July 5, 2023")) / years()

#Converting the year calculations to an integer
USA_DOB %--% mdy("July 3, 2023") %/% years()
USA_DOB %--% mdy("July 5, 2023") %/% years()

#Creating a function automating the calendar time conversion
age <- function(birthdate) {
  birthdate %--% today() %/% years()
}

#Example using age function
age(dmy("27/06/2000"))
