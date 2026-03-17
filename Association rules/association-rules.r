#Association Rules

#Collecting, exploring and preparing data

#Load library arules
library(arules)

#Load csv into a sparse matrix
groceries <- read.transactions("groceries.csv", sep = ",")

#Viewing basic information about spare matrix
summary(groceries)

#Representing transactions in long format
head(toLongFormat(groceries, decode = FALSE), n = 7)

#Exploring first five transactions in sparse matrix
inspect(groceries[1:5])

#Exploring frequency of items bought by percentage
itemFrequency(groceries[, 1:3])

#Visualising data of proportion of items bought, with 10% min threshold
itemFrequencyPlot(groceries, support = 0.1)

#Visualising data of 20 most bought items
itemFrequencyPlot(groceries, topN = 20)

#Visualising first five transactions in sparse matrix
image(groceries[1:5])

#Visualising sparse matrix for 100 random transactions
image(sample(groceries, 100))
