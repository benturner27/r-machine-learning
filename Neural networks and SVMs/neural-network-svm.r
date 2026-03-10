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

#Training model on data

#Load library neuralnet
library(neuralnet)

#Implementing set.seed to ensure the same result is produced
set.seed(12345)

#Training the model using a single hidden node and single output node
concrete_model <- neuralnet(strength ~ cement + slag +
                              ash + water + superplastic +
                              coarseagg + fineagg + age, 
                            data = concrete_train)

#Visualising single hidden layer node neural network
plot(concrete_model)

#Evaluating model performance

#Using trained model to predict test data
model_results <- compute(concrete_model, concrete_test[1:8])

#Retrieving model results as a variable
predicted_strength <- model_results$net.result

#Calculating correlation between predicted values and actual values
cor(predicted_strength, concrete_test$strength)
