#Neural Networks and Support Vector Machines (SVMs)

#Collecting, exploring and preparing data

#Read csv to environment and exploring data frame structure
concrete <- read.csv("concrete.csv")
str(concrete)

#creating data normalisation function because values are too extreme for neural 
#network to predict effectively
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#Applying function to data frame
concrete_norm <- as.data.frame(lapply(concrete, normalise))

#Verifying values have been normalised in comparison to original raw data
summary(concrete_norm$strength)
summary(concrete$strength)

#Splitting the normalised data frame into training and testing sets
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]
