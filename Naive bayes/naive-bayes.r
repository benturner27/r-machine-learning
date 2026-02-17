#Naive Bayes

#Exploring and preparing data

#Load csv file into environment
sms_raw <- read.csv("sms_spam.csv")

#Examine observations and variables using str function
str(sms_raw)

#Convert type variable into factor
sms_raw$type <- factor(sms_raw$type)

#Examine type factor using str and table functions
str(sms_raw$type)
table(sms_raw$type)
