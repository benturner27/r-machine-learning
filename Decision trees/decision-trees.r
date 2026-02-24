#Decision Trees

#Understanding decision trees - Entropy example

#Calculating the entropy between two classes
#Where the dataset is split between red (60%) and white (40%)
-0.60 * log2(0.60) - 0.40 * log2(0.40)

#Visualising entropy levels for all two-class arrangements using curve function
curve(-x * log2(x) - (1 - x) * log2(1 - x),
      col = "red", xlab = "x", ylab = "entropy", lwd = 4)

#Data Preparation

#Data analysis

#Read csv to file, enabling StringAsFactors parameter to true, since all data is
#categorical
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
str(credit)

#Exploring trends for loans by analysing bank accounts and savings using table
#function
table(credit$checking_balance)
table(credit$savings_balance)

#Analysing loan duration and loan amount using summary function
summary(credit$months_loan_duration)
summary(credit$amount)

#Checking the proportion of applicants have defaulted using table
table(credit$default)

