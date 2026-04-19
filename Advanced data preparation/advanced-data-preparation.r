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
