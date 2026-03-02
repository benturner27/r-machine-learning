#Regression methods

#Single Linear Regression

#Example: Estimating a regression line on launch shuttle data

#Read csv to envirnonment
launch <- read.csv("challenger.csv")

#Calculation the value of b by dividing the covariance by variance
b <- cov(launch$temperature, launch$distress_ct) /
      var(launch$temperature)
b

#Calculation the value of a by using b and applying mean function
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

#Calculating correlation between launch temperature and O-ring distress events
r <- cov(launch$temperature, launch$distress_ct) /
      (sd(launch$temperature) * sd(launch$distress_ct))
r

#Alternative method using cor function
cor(launch$temperature, launch$distress_ct)


