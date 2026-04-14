#Success in Machine Learning

#Exploratory data analysis

#Loading csv to environment and exploring its features
titanic_train <- read.csv("titanic_train.csv")
str(titanic_train)

#Assigning ggplot as a variable
library(ggplot2)
p <- ggplot(data = titanic_train)
p

#creating a box plot using R's built-in package
boxplot(titanic_train$Age)

#Creating a box plot using ggplot2
p + geom_boxplot(mapping = aes(y = Age))

#Creating a bivariate box plot using ggplot2
p + geom_boxplot(aes(x = Age, y = as.factor(Survived)))

#Creating a histogram using R's built-in package
hist(titanic_train$Age)

#Creating a histogram using ggplot2
p + geom_histogram(aes(x = Age))

#Creating a histogram that highlights the number of passengers survived
p + geom_histogram(aes(x = Age, fill = as.factor(Survived))) +
  ggtitle("Distribution of Age by Titanic Survival status")

#Creating a side-by-side histogram comparing the distribution of survival status
p + geom_histogram(aes(x = Age)) +
  facet_grid(col = vars(Survived)) +
  ggtitle("Distribution of Age by Titanic Survival Status")

#Creating a density plot showing the distribution of survival status by age
p + geom_density(aes(x = Age,
                     color = as.factor(Survived),
                     fill = as.factor(Survived)),
                 alpha = 0.25) +
  ggtitle("Density of Age by Titanic Survival Status")

