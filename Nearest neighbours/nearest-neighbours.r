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

#Training model on data

#loading library class
library(class)

#modelling training data using knn function
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

#Data evaluation

#loading library gmodels
library(gmodels)

#Create CrossTable for cross-tabulating model prediction with labelled data
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

#Improving prediction score

#Standardising all numerical data of data frame with z-score standardisation
#using scale function
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

#Implement the standardised numerical data into modelling and evaluating
wbcd_train_z <- wbcd_z[1:469, ]
wbcd_test_z <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

#K score opimisation

#Creating a for loop where the model loops different k numbers to find most
#efficient model
k_values <- c(1, 5, 11, 15, 21, 27)
for (k_val in k_values) {
  wbcd_test_pred <- knn(train = wbcd_train,
                       test = wbcd_test,
                       cl = wbcd_train_labels,
                       k = k_val)
  CrossTable(x = wbcd_test_labels,
             y = wbcd_test_pred,
             prop.chisq = FALSE)
}
