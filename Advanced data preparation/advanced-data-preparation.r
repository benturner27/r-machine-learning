#Advanced Data Preparation

#Table structures

#Creating a tibble using titanic.csv
library(tibble)
titanic_csv <- read.csv("titanic_train.csv")
titanic_tbl <- as_tibble(titanic_csv)
