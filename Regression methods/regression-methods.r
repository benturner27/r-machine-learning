#Regression methods

#Single Linear Regression

#Example: Estimating a regression line on launch shuttle data

#Read csv to environment
launch <- read.csv("challenger.csv")

#Calculating value of b by dividing the covariance by variance
b <- cov(launch$temperature, launch$distress_ct) /
      var(launch$temperature)
b

#Calculating value of a by using b and applying mean function
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

#Calculating correlation between launch temperature and O-ring distress events
r <- cov(launch$temperature, launch$distress_ct) /
      (sd(launch$temperature) * sd(launch$distress_ct))
r

#Alternative method using cor function
cor(launch$temperature, launch$distress_ct)

#Multiple Linear Regression

#Creating regression function reg, takes in y & x parameters, returns vector of
#estimated beta coefficients
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*%t(x) %*% y
  colnames = "estimate"
  print(b)
}
#New command key
#as.matrix converts data frame into matrix
#cbind binds additional column on x matrix,
#Intercept = 1 renames new column, fills column with 1 values

#Matrix operations key
#solve() takes inverse of matrix
#t transposes matrix
#%*% multiplies two matrices

#analysing data structure of launch data frame
str(launch)

#Using reg function to calculate simple regression model as done earlier
reg(y = launch$distress_ct, x = launch[2])

#Using reg function to calculate multiple regression module, multiplying
#distress_ct by other three variables in data frame
reg(y = launch$distress_ct, x = launch[2:4])

#------------------------------------------------------------------------------

#Generalised Linear Models and Logistic Regression

#Collecting and Preparing Data

#load csv into environment
insurance <- read.csv("autoinsurance.csv", stringsAsFactors = TRUE)
str(insurance)

#Using summary to check for normality under expenses variable
summary(insurance$expenses)

#Verifying data is right-skewed by creating histogram
hist(insurance$expenses)

#Exploring the distribution of geo_area and vehicle_type variables
table(insurance$geo_area)
table(insurance$vehicle_type)

#Exploring relationships with features

#Correlation matrix

#Creating a correlation matrix of four non-numeric variables using cor function
cor(insurance[c("age", "est_value", "miles_driven", "expenses")])

#Scatterplot Matrix

#Creating a scatterplot matrix of the four variables from ealier using pairs
#function to detect patterns and potential relationship in the data
pairs(insurance[c("age", "est_value", "miles_driven", "expenses")],
      pch = ".")

#Loading library psych
library(psych)

#using pairs.panels function to create more insightful scatterplot matrix
pairs.panels(insurance[c("age", "est_value", "miles_driven", "expenses")],
      pch = ".")

#Training model on data

#For specifying certain independent values to train against the dependent value
ins_model <- lm(expenses ~ age + geo_area + vehicle_type + 
                  est_values + miles_driven + college_grad_ind +
                  sppeding_ticket_ind + hard_braking_ind +
                  late_driving_ind + clean_driving_ind,
                data = insurance)

#For training all features against dependent value
ins_model <- lm(expenses ~ ., data = insurance)

#Calling model to view estimated beta coefficients
options(scipen = 999) #turns off scientific notation, easier to read
ins_model

#Evaluating model performance

#Using summary to evaluate the model's performance and how well it fits the data
summary(ins_model)

#Improving Model Performance

#Non-linear relationships

#Creating age-squared variable to data frame in order to add higher order term
insurance$age2 <- insurance$age^2

#Training second model including higher order term and intreaction between
#hard_braking_ind and late_driving_ind
ins_model2 <- lm(expenses ~ . + hard_braking_ind:late_driving_ind,
                 data = insurance)

#Viewing summary to see if model has improved performance
summary(ins_model2)

#Making Predictions

#Adding prediction model to data frame
insurance$pred <- predict(ins_model2, insurance)

#Calculating correlation between predicted and actual expenses
cor(insurance$pred, insurance$expenses)

#Visualising relationship using scatterplot
plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)

#Predicting a new prospective customer: 30 y/o truck driver, vehicle valued at
#$25,000, driven 14,000 miles and clean driving record
predict(ins_model2, 
        data.frame(age = 30, age2 = 30^2, geo_area = "rural",
                   vehicle_type = "truck", est_value = 25000,
                   miles_driven = 14000, college_grad_ind = 0,
                   speeding_ticket_ind = 0, hard_braking_ind = 0,
                   late_driving_ind = 0, clean_driving_ind = 1))

#Predicting similar driver with same values, but has had an accident recently
predict(ins_model2, 
        data.frame(age = 30, age2 = 30^2, geo_area = "rural",
                   vehicle_type = "truck", est_value = 25000,
                   miles_driven = 14000, college_grad_ind = 0,
                   speeding_ticket_ind = 0, hard_braking_ind = 0,
                   late_driving_ind = 0, clean_driving_ind = 0))

#Predicting similar driver with same values, but has drive 10,000 more miles
predict(ins_model2, 
        data.frame(age = 30, age2 = 30^2, geo_area = "rural",
                   vehicle_type = "truck", est_value = 25000,
                   miles_driven = 24000, college_grad_ind = 0,
                   speeding_ticket_ind = 0, hard_braking_ind = 0,
                   late_driving_ind = 0, clean_driving_ind = 0))

#Confirming that the difference in premiums between the second and third
#predictions will give the same total as the estimated coefficient for
#miles_driven
2435.384 - 1247.903

#------------------------------------------------------------------------------

#Predicting Policyholder churn with Logistic Regression

#Reading csv to environment
churn_data <- read.csv("insurance_churn.csv")

#Using prop.table to view the overall churn rate
prop.table(table(churn_data$churn))

#Training a general linear regression model to the churn data frame
churn_model <- glm(churn ~ . -member_id, data = churn_data,
                   family = binomial(link = "logit"))

#Viewing the estimated regression parameters of the model using summary
summary(churn_model)

#Loading testing data to environment
churn_test <- read.csv("insurance_churn_test.csv")

#Adding prediction model to test data frame
churn_test$churn_prob <- predict(churn_model, churn_test, 
                                 type = "response")
#Using summary to view the prediction results 
summary(churn_test$churn_prob)

#Using order function to make new vector with descending order of churn
#probability (highest to lowest)
churn_order <- order(churn_test$churn_prob, decreasing = TRUE)

#Using head function to take n rows for top 5 members most likely to churn
#including member_id values
head(churn_test[churn_order, c("member_id", "churn_prob")], n = 5)

#------------------------------------------------------------------------------

#Regression Trees and Numeric Trees

#Example calculating Numeric Decision Trees

#Initialising sets based on feature splits on two different points
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)

#Calculating Standard Deviation Reduction (SDR) of both feature split variants
sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) +
                      length(at2) / length(tee) * sd(at2))
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) +
                      length(bt2) / length(tee) * sd(bt2))
sdr_a
sdr_b
