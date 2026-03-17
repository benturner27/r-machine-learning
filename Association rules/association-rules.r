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

#Training model on data

#Implementing model on transactional data
groceryrules <- apriori(groceries, parameter = list(support = 0.006,
                        confidence = 0.25, minlen = 2))

#Evaluating model performance

#Exploring the overview of trained model
summary(groceryrules)

#Inspecting the association rules determined by model
inspect(groceryrules[1:3])

#Improving model performance

#Sorting learned rules by magnitude of lift
inspect(sort(groceryrules, by = "lift")[1:5])

#Creating a variable for rules associated with berries
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

#writing learned rules to csv file
write(groceryrules, file = "groceryrules.csv", sep = ",",
      quote = TRUE, row.names = TRUE)

#Converting rules to data frame
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)

#Training eclat model on transactional data to determine support data
groceryrules_eclat <- eclat(groceries, support = 0.006)

#Inspecting the model decisions on support
inspect(groceryrules_eclat[1:5])

#Training model to determine confidence data
groceryrules_eclat <- ruleInduction(groceryrules_eclat, confidence = 0.25)

#Inspecting the rules determined
inspect(groceryrules_eclat[1:5])
