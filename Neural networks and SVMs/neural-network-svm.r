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

#Visualising single hidden layer node neural network topology
plot(concrete_model)

#Evaluating model performance

#Using trained model to predict test data
model_results <- compute(concrete_model, concrete_test[1:8])

#Retrieving model results as a variable
predicted_strength <- model_results$net.result

#Calculating correlation between predicted values and actual values
cor(predicted_strength, concrete_test$strength)

#Improving model performance

#Increasing the nodes in hidden layer to 5
set.seed(12345)
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic +
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)

#Visualising second model topology
plot(concrete_model2)

#Using trained model to predict strength values and evaluate performance
model_results2 <- compute(concrete_model2, concrete_test)
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

#Defining a SoftPlus function to be used as the activation function
softplus <- function(x) {
  log(1 + exp(x))
}

#Building third model using softplus function
set.seed(12345)
concrete_model3 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic +
                               coarseagg + fineagg + age,
                             data = concrete_train, 
                             hidden = c(5, 5),
                             act.fct = softplus)

#visualising topology of third model
plot(concrete_model3)

#Predicting and evaluating model performance
model_results3 <- compute(concrete_model3, concrete_test)
predicted_strength3 <- model_results3$net.result
cor(predicted_strength3, concrete_test$strength)

#Creating data frame converting model predicted values to compare with actual
#data values
strengths <- data.frame(
  actual = concrete$strength[774:1030],
  predicted = predicted_strength3
)
strengths

#Creating unnormalise function to convert predicted results to format of actual
#data
unnormalise <- function(x) {
  return(x * (max(concrete$strength) - min(concrete$strength))
              + min(concrete$strength))
}

#Implementing function to predicted results
strengths$pred_new <- unnormalise(strengths$pred)

#calculating error percentage between predicted and actual strength values
strengths$error_pct <- (strengths$pred_new - strengths$actual) / 
  strengths$actual
strengths

#Verifying correlation between actual values and predicted values using
#original data format
cor(strengths$pred_new, strengths$actual)

#------------------------------------------------------------------------------

#Support Vector Machines

#Collecting, exploring and preparing data

#write CSV to document and examining document format
letters <- read.csv("letterdata.csv", stringsAsFactors = TRUE)
str(letters)

#Splitting the data frame into training and esting sets
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

#Training model on data

#Load library kernlab
library(kernlab)

#Training model on data
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")
letter_classifier
