#Nearest Neighbours

#Read the csv file into the environment
wbcd <- read.csv("wisc_bc_data.csv")

#Examining wbcd data frame using str function - lists off all variables in
#data frame
str(wbcd)

#Remove the id variable from data frame - does not provide any useful data
wbcd <- wbcd[-1]

#Show data of diagnosis variable using table function - allows us to evaluate
#accuracy of training later on
table(wbcd$diagnosis)

#Renaming diagnosis values for better legibility recoding them as factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

#Convert the values of each label to a percentage using round function
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#Showing summary of three variables in data frame using summary function
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#Normalising numerical data

#Creating normalise function in order to normalise all numerical data - limits
#big numbers from influencing training model
normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#example data using normalise function
normalise(c(1, 2, 3, 4, 5))
normalise(c(20, 40, 60, 80, 100))

#Transforming all 30 numerical variables to normalised values, using lapply
#function, as well as as.data.frame function to turn it into a data frame
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalise))

#confirming numerical data is normalised by checking summary function
summary(wbcd_n$area_mean)

#Data Preparation

#Splitting data frame into training and testing batches for modelling
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

#Create batches from original data frame with only the diagnosis column
#Will be used for evaluation of model

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]













