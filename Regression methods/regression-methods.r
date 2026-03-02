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
