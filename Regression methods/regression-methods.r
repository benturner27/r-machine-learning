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
